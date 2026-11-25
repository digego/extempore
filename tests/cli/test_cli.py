"""The extempore command line, run as a user runs it: what each subcommand
puts on stdout and stderr, and how it exits.

EXTEMPORE_BIN names the binary (ctest sets it); the default is build/extempore.
"""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
BIN = Path(os.environ.get("EXTEMPORE_BIN", REPO / "build" / "extempore")).resolve()


def run(
    *args: str, timeout: float = 120, env: dict[str, str] | None = None
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(BIN), *args], cwd=REPO, capture_output=True, text=True, timeout=timeout, env=env
    )


class Scratch(unittest.TestCase):
    def setUp(self) -> None:
        self._dir = tempfile.TemporaryDirectory()
        self.dir = Path(self._dir.name)

    def tearDown(self) -> None:
        self._dir.cleanup()

    def write(self, name: str, text: str) -> str:
        path = self.dir / name
        path.write_text(text)
        return str(path)


class Exec(Scratch):
    def test_stdout_is_what_the_code_prints(self) -> None:
        p = run("exec", "-e", "(println (+ 1 2))")
        self.assertEqual((p.returncode, p.stdout, p.stderr), (0, "3\n", ""))
        p = run("exec", "-v", "-e", "(println (+ 1 2))")
        self.assertEqual(p.stdout, "3\n")
        self.assertIn("Loaded xtmbase", p.stderr)  # the load message is a message

    def test_compiler_messages_go_to_stderr_with_v(self) -> None:
        file = self.write(
            "sq.xtm",
            "(bind-func sq (lambda (x:i64) (* x x)))\n(println 'square (sq 12))\n",
        )
        p = run("exec", "-v", file)
        self.assertEqual(p.returncode, 0)
        self.assertEqual(p.stdout, "square 144\n")
        self.assertIn("Compiled: sq >>> [i64,i64]*", p.stderr)

    def test_levels_and_filters_choose_the_messages(self) -> None:
        code = (
            "(bind-func cube (lambda (x:i64) (* x x x)))"
            '(log:warn "careful") (log:emit (quote debug) (quote midi) "deep")'
        )
        p = run("exec", "-e", code)
        self.assertEqual(p.stderr, "warning: careful\n")
        self.assertEqual(run("exec", "-q", "-e", code).stderr, "")
        verbose = run("exec", "-vv", "-e", code).stderr
        self.assertIn("Compiled: cube", verbose)
        self.assertIn("debug: midi: deep\n", verbose)
        env = {**os.environ, "EXTEMPORE_LOG": "info,compiler=warning,midi=debug"}
        filtered = run("exec", "-e", code, env=env).stderr
        self.assertNotIn("Compiled", filtered)
        self.assertIn("Loaded xtmbase", filtered)
        self.assertIn("debug: midi: deep\n", filtered)
        bad = run("exec", "-e", "(+ 1 1)", env={**os.environ, "EXTEMPORE_LOG": "loud"})
        self.assertEqual(bad.returncode, 0)
        self.assertIn('EXTEMPORE_LOG: can\'t read "loud"', bad.stderr)

    def test_timestamps_when_asked(self) -> None:
        p = run("exec", "--timestamps", "always", "-e", '(log:warn "now")')
        self.assertRegex(p.stderr, r"^\d\d:\d\d:\d\d\.\d\d\d warning: now\n$")

    def test_no_colour_through_a_pipe_unless_asked(self) -> None:
        code = '(log:warn "careful")'
        self.assertNotIn("\x1b[", run("exec", "-e", code).stderr)
        self.assertIn("\x1b[", run("exec", "--color", "always", "-e", code).stderr)
        p = run("exec", "--color", "always", "-e", '(print-with-colors (quote red) (quote default) #t (print "red"))')
        self.assertIn("\x1b[", p.stdout)

    def test_a_missing_file_is_status_1(self) -> None:
        p = run("exec", "-e", '(sys:load "no-such-file.xtm") (println (quote after))')
        self.assertEqual((p.returncode, p.stdout), (1, "after\n"))
        self.assertIn("error: sys:load: can't find no-such-file.xtm", p.stderr)

    def test_scheduled_work_finishes_before_it_exits(self) -> None:
        file = self.write(
            "later.xtm",
            """(define n 0)
(define tick
  (lambda ()
    (set! n (+ n 1))
    (if (< n 4) (callback (+ (now) 2000) 'tick) (println 'ticks n))))
(tick)
(callback (+ (now) 22050) (lambda () (println 'later)))
""",
        )
        p = run("exec", "--no-base", file)
        self.assertEqual(p.returncode, 0)
        self.assertEqual(p.stdout, "ticks 4\nlater\n")

    def test_arguments_after_the_double_dash_reach_the_code(self) -> None:
        p = run(
            "exec",
            "--no-base",
            "-e",
            '(println (sys:cmdarg "label") (sys:cmdarg "missing"))',
            "--",
            "--label=one two",
        )
        self.assertEqual(p.returncode, 0)
        self.assertEqual(p.stdout, '"one two" #f\n')

    def test_an_error_in_a_file_is_status_1_and_the_rest_still_runs(self) -> None:
        file = self.write("err.xtm", "(println 1)\n(car 5)\n(println 2)\n")
        p = run("exec", "--no-base", file)
        self.assertEqual(p.returncode, 1)
        self.assertEqual(p.stdout, "1\n2\n")
        self.assertIn("car argument", p.stderr)
        self.assertIn("at " + file + ":2", p.stderr)

    def test_an_error_in_an_expression_is_status_1(self) -> None:
        p = run("exec", "--no-base", "-e", "(car 5)")
        self.assertEqual(p.returncode, 1)
        self.assertEqual(p.stdout, "")
        self.assertIn("car argument", p.stderr)

    def test_an_error_in_a_callback_is_status_1(self) -> None:
        p = run(
            "exec", "--no-base", "-e", "(callback (+ (now) 1000) (lambda () (car 5)))"
        )
        self.assertEqual(p.returncode, 1)
        self.assertIn("car argument", p.stderr)

    def test_a_replaced_closure_does_not_hold_the_run_open(self) -> None:
        # the old closure's zone is freed ten seconds later, by a task which
        # nothing waits for; the callback is still waited for
        code = """(begin (bind-func f (lambda () 1))
                         (bind-func f (lambda () 2))
                         (callback (+ (now) 22050) (lambda () (println (f)))))"""
        start = time.monotonic()
        p = run("exec", "--no-base", "-e", code)
        self.assertEqual(p.returncode, 0, p.stderr)
        self.assertEqual(p.stdout, "2\n")
        self.assertLess(time.monotonic() - start, 6)

    def test_an_xtlang_compile_error_is_status_1(self) -> None:
        p = run("exec", "-e", '(bind-func bad (lambda (x:i64) (+ x "s")))')
        self.assertEqual(p.returncode, 1)
        self.assertEqual(p.stdout, "")
        self.assertIn("error: couldn't resolve the type of bad\n    ", p.stderr)

    def test_sys_sleep_resumes_whatever_stdin_is(self) -> None:
        # sys:sleep escapes through a continuation which once left s7 reading
        # its next form from stdin: at once from a closed stdin, never from an
        # idle pipe or a terminal
        p = subprocess.Popen(
            [
                str(BIN),
                "exec",
                "--no-base",
                "-e",
                "(begin (sys:sleep 4410) (println 'woke))",
            ],
            cwd=REPO,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
        )
        try:
            # stdin stays open and silent for as long as it runs
            self.assertEqual(p.wait(timeout=60), 0)
            self.assertEqual(p.stdout.read(), "woke\n")
        finally:
            p.stdin.close()
            p.stdout.close()
            if p.poll() is None:
                p.kill()
                p.wait()

    def test_quit_sets_the_status(self) -> None:
        self.assertEqual(run("exec", "--no-base", "-e", "(quit 7)").returncode, 7)

    def test_wait_keeps_running_until_quit(self) -> None:
        p = subprocess.Popen(
            [str(BIN), "exec", "--no-base", "--wait", "-e", "(println 'ready)"],
            cwd=REPO,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
        )
        try:
            self.assertEqual(p.stdout.readline(), "ready\n")
            time.sleep(1.5)
            self.assertIsNone(p.poll(), "exec --wait exited with nothing left to run")
        finally:
            p.kill()
            p.wait()
            p.stdout.close()


class Session(Scratch):
    QUIET_SESSION = ("--no-audio", "--no-nrepl", "--no-legacy-server")

    def test_messages_on_stderr_and_the_code_on_stdout(self) -> None:
        p = run(*self.QUIET_SESSION, "-e", "(begin (println 'ready) (quit 0))")
        self.assertEqual((p.returncode, p.stdout), (0, "ready\n"))
        # stderr isn't a terminal here, so each line has the time
        self.assertRegex(p.stderr, r"^\d\d:\d\d:\d\d\.\d{3} Extempore v")
        self.assertIn("Loaded xtmbase", p.stderr)
        p = run(*self.QUIET_SESSION, "-q", "-e", "(begin (println 'ready) (quit 0))")
        self.assertEqual((p.returncode, p.stdout, p.stderr), (0, "ready\n", ""))

    def test_timestamps_can_be_turned_off(self) -> None:
        p = run(*self.QUIET_SESSION, "--timestamps", "never", "-e", "(quit 0)")
        self.assertTrue(p.stderr.startswith("Extempore v"), p.stderr)


class Devices(Scratch):
    def test_no_colour_through_a_pipe(self) -> None:
        p = run("devices")
        self.assertNotIn("\x1b[", p.stdout + p.stderr)


class Usage(Scratch):
    def assert_usage_error(self, *args: str, mentions: str) -> None:
        p = run(*args)
        self.assertEqual(p.returncode, 2, p.stderr)
        self.assertEqual(p.stdout, "")
        self.assertIn(mentions, p.stderr)

    def test_usage_errors_are_status_2(self) -> None:
        self.assert_usage_error(
            "--sampelrate", "48000", mentions="unknown option --sampelrate"
        )
        self.assert_usage_error("exec", mentions="a file or -e")
        self.assert_usage_error("exec", "nosuch.xtm", mentions="can't find nosuch.xtm")
        self.assert_usage_error(
            "render", "examples/core/hello-sine.xtm", mentions="-o OUT.wav"
        )
        self.assert_usage_error("--sample-rate", "fast", mentions="whole number")
        self.assert_usage_error("--latency", "99999999999", mentions="too large")
        self.assert_usage_error("--timediv", "4", mentions="is gone")
        self.assert_usage_error("--runtime", "/x", mentions="--share-dir")

    def test_version_and_help(self) -> None:
        p = run("version")
        self.assertEqual(p.returncode, 0)
        # a tag and what follows it, or in a checkout without tags a bare hash
        self.assertRegex(p.stdout, r"^Extempore \S+\n$")
        p = run("--help")
        self.assertEqual(p.returncode, 0)
        for subcommand in ("exec", "test", "render", "compile", "devices"):
            self.assertIn(f"\n  {subcommand}", p.stdout)
        p = run("render", "--help")
        self.assertIn("--duration SECONDS", p.stdout)
        self.assertNotIn("--device", p.stdout)

    def test_a_renamed_option_works_and_says_its_new_name(self) -> None:
        p = run("exec", "--nobase", "-e", "(println 1)")
        self.assertEqual(p.returncode, 0)
        self.assertEqual(p.stdout, "1\n")
        self.assertIn("--nobase is now --no-base", p.stderr)


class OldModeOptions(Scratch):
    """A command line with an old mode option runs as the subcommand which
    replaces it, and says so."""

    def test_batch_is_exec_wait(self) -> None:
        p = run("--nobase", "--batch", "(begin (println 'hello) (quit 3))")
        self.assertEqual(p.returncode, 3)
        self.assertEqual(p.stdout, "hello\n")
        self.assertIn("--batch is deprecated", p.stderr)
        self.assertIn("extempore exec", p.stderr)

    def test_an_unknown_option_is_an_error_there_too(self) -> None:
        p = run("--nobase", "--label=x", "--batch", "(quit 0)")
        self.assertEqual(p.returncode, 2)
        self.assertIn("unknown option --label", p.stderr)

    def test_audio_outfile_is_render(self) -> None:
        wav = self.dir / "old.wav"
        p = run(
            "--run",
            "examples/core/hello-sine.xtm",
            "--audio-outfile",
            str(wav),
            "--duration",
            "0.5",
        )
        self.assertEqual(p.returncode, 0, p.stderr)
        self.assertEqual(wav.stat().st_size, 44 + 22050 * 2 * 4)
        self.assertIn("--audio-outfile is deprecated", p.stderr)


NOISE = """(bind-val amp SAMPLE 0.1)
(bind-func dsp:DSP
  (lambda (in:SAMPLE time:i64 channel:i64 data:SAMPLE*)
    (* amp (- (* 2.0 (random)) 1.0))))
(bind-func set_amp (lambda (a:SAMPLE) (set! amp a) void))
(dsp:set! dsp)
(define wobble (lambda () (set_amp (* 0.5 (random))) (callback (+ (now) 3000) 'wobble)))
(wobble)
"""


class Render(Scratch):
    def render(
        self, file: str, out: str, *args: str
    ) -> subprocess.CompletedProcess[str]:
        return run("render", file, "-o", str(self.dir / out), "--duration", "1", *args)

    def test_writes_exactly_the_duration_and_nothing_on_stdout(self) -> None:
        file = self.write("noise.xtm", NOISE)
        p = self.render(file, "a.wav")
        self.assertEqual(p.returncode, 0, p.stderr)
        self.assertEqual(p.stdout, "")
        self.assertEqual((self.dir / "a.wav").stat().st_size, 44 + 44100 * 2 * 4)
        self.assertIn("wrote", p.stderr)
        self.assertNotIn("warning", p.stderr)

    def test_with_no_dsp_the_clock_still_runs_and_it_warns_of_silence(self) -> None:
        file = self.write(
            "nodsp.xtm", "(callback (+ (now) 22050) (lambda () (println 'at (now))))\n"
        )
        p = self.render(file, "silence.wav")
        self.assertEqual(p.returncode, 0, p.stderr)
        self.assertEqual(p.stdout, "at 21504\n")  # the block which holds frame 22050
        data = (self.dir / "silence.wav").read_bytes()
        self.assertEqual(len(data), 44 + 44100 * 2 * 4)
        self.assertFalse(any(data[44:]))
        self.assertIn("no DSP was set", p.stderr)

    def test_the_same_seed_gives_the_same_samples(self) -> None:
        file = self.write("noise.xtm", NOISE)
        for out, seed in (("a.wav", "0"), ("b.wav", "0"), ("c.wav", "9")):
            self.assertEqual(self.render(file, out, "--seed", seed).returncode, 0)
        a, b, c = ((self.dir / n).read_bytes() for n in ("a.wav", "b.wav", "c.wav"))
        self.assertEqual(a, b)
        self.assertNotEqual(a, c)
        self.assertGreater(len(set(a[44:4044])), 16, "the render is silent")

    def test_an_error_is_status_1_and_the_file_is_still_written(self) -> None:
        file = self.write("bad.xtm", NOISE + "(car 5)\n")
        p = self.render(file, "bad.wav")
        self.assertEqual(p.returncode, 1)
        self.assertIn("car argument", p.stderr)
        self.assertEqual((self.dir / "bad.wav").stat().st_size, 44 + 44100 * 2 * 4)

    def test_quit_ends_it_early_with_a_whole_file(self) -> None:
        file = self.write("short.xtm", NOISE + "(callback (+ (now) 11025) quit 0)\n")
        p = self.render(file, "short.wav")
        self.assertEqual(p.returncode, 0)
        size = (self.dir / "short.wav").stat().st_size
        self.assertEqual(
            size, 44 + 10240 * 2 * 4
        )  # the ten blocks before the one with the quit


class Test(Scratch):
    PASSING = "(xtmtest-result (+ 1 1) 2)\n"
    FAILING = "(xtmtest-result (+ 1 1) 3)\n"

    def test_status_follows_the_assertions(self) -> None:
        self.assertEqual(
            run("test", self.write("pass.xtm", self.PASSING)).returncode, 0
        )
        self.assertEqual(
            run("test", self.write("fail.xtm", self.FAILING)).returncode, 1
        )

    def test_several_files_each_run_and_are_summed_up(self) -> None:
        files = [
            self.write("a.xtm", self.PASSING),
            self.write("b.xtm", self.FAILING),
            self.write("c.xtm", "(define from-c 1)\n" + self.PASSING),
            # a definition from the file before isn't there: its own process
            self.write("d.xtm", "(xtmtest-result (defined? 'from-c) #f)\n"),
        ]
        junit = self.dir / "reports"
        p = run("test", *files, "--junit", str(junit))
        self.assertEqual(p.returncode, 1)
        self.assertIn("4 test files, 1 failed", p.stdout)
        self.assertIn("b.xtm", p.stdout.split("4 test files")[1])
        self.assertEqual(len(list(junit.glob("*.xml"))), 4)



class Fmt(Scratch):
    """extempore fmt: whitespace only, files in place or stdin to stdout."""

    MESSY = "(define (f x)\n        (+ x 1))   \n\n\n"
    TIDY = "(define (f x)\n  (+ x 1))\n"

    def test_stdin_to_stdout(self) -> None:
        p = subprocess.run([str(BIN), "fmt"], input=self.MESSY, capture_output=True, text=True)
        self.assertEqual((p.returncode, p.stdout, p.stderr), (0, self.TIDY, ""))

    def test_files_and_directories_in_place(self) -> None:
        a = self.write("a.xtm", self.MESSY)
        (self.dir / "sub").mkdir()
        b = self.write("sub/b.xtm", self.MESSY)
        other = self.write("sub/notes.txt", self.MESSY)
        p = run("fmt", a, str(self.dir / "sub"))
        self.assertEqual((p.returncode, p.stdout, p.stderr), (0, "", ""))
        self.assertEqual(Path(a).read_text(), self.TIDY)
        self.assertEqual(Path(b).read_text(), self.TIDY)
        self.assertEqual(Path(other).read_text(), self.MESSY)

    def test_check_changes_nothing_and_lists_what_it_would(self) -> None:
        messy = self.write("messy.xtm", self.MESSY)
        tidy = self.write("tidy.xtm", self.TIDY)
        p = run("fmt", "--check", messy, tidy)
        self.assertEqual((p.returncode, p.stdout), (1, messy + "\n"))
        self.assertEqual(Path(messy).read_text(), self.MESSY)
        self.assertEqual(run("fmt", "--check", tidy).returncode, 0)

    def test_a_file_which_doesnt_read_is_left_alone(self) -> None:
        broken = self.write("broken.xtm", "(define (f x)\n  (+ x 1)\n")
        tidy = self.write("tidy.xtm", self.MESSY)
        p = run("fmt", broken, tidy)
        self.assertEqual(p.returncode, 1)
        self.assertIn("broken.xtm: line 1, column 1: this form never ends", p.stderr)
        self.assertEqual(Path(broken).read_text(), "(define (f x)\n  (+ x 1)\n")
        self.assertEqual(Path(tidy).read_text(), self.TIDY)


if __name__ == "__main__":
    unittest.main()

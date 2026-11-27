"""The clients built into the binary: extempore send, extempore status and
extempore repl, each run as a user runs it against a running extempore."""

from __future__ import annotations

import os
import select
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path

from harness import BIN, Extempore


class Clients(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.xtm = Extempore()  # --nrepl-port 0: it writes .nrepl-port in its cwd

    @classmethod
    def tearDownClass(cls) -> None:
        cls.xtm.stop()

    def run_client(self, *args: str, stdin: str | None = None, cwd: Path | None = None,
                   env: dict[str, str] | None = None, port: bool = True
                   ) -> subprocess.CompletedProcess[str]:
        cmd = [str(BIN), *args]
        if port:
            cmd += ["--port", str(self.xtm.port)]
        environment = {k: v for k, v in os.environ.items() if k != "EXTEMPORE_NREPL_PORT"}
        environment.update(env or {})
        return subprocess.run(
            cmd, input=stdin, stdin=None if stdin is not None else subprocess.DEVNULL,
            capture_output=True, text=True, timeout=60, cwd=cwd, env=environment,
        )


class Send(Clients):
    def test_output_and_values_on_stdout(self) -> None:
        p = self.run_client("send", '(begin (println "hi") (+ 40 2))')
        self.assertEqual((p.returncode, p.stdout, p.stderr), (0, '"hi"\n42\n', ""))

    def test_code_from_stdin(self) -> None:
        p = self.run_client("send", stdin="(* 6 7)\n")
        self.assertEqual((p.returncode, p.stdout), (0, "42\n"))

    def test_an_error_is_status_1_on_stderr(self) -> None:
        p = self.run_client("send", "(car 1)")
        self.assertEqual(p.returncode, 1)
        self.assertEqual(p.stdout, "")
        self.assertIn("car argument, 1, is an integer but should be a pair", p.stderr)

    def test_nothing_to_send_is_status_2(self) -> None:
        p = self.run_client("send", stdin="  \n")
        self.assertEqual(p.returncode, 2)
        self.assertIn("nothing to send", p.stderr)

    def test_the_form_at_a_position(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            song = Path(d) / "song.xtm"
            song.write_text('(define x 1)\n\n(begin\n  (println "hi")\n  (+ 40 2))\n')
            p = self.run_client("send", "--file", str(song), "--line", "4", "--column", "5")
        self.assertEqual((p.returncode, p.stdout), (0, '"hi"\n42\n'))

    def test_errors_carry_the_files_line(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            song = Path(d) / "song.xtm"
            song.write_text("(define x 1)\n\n\n(car 1)\n")
            p = self.run_client("send", "--file", str(song), "--line", "4")
        self.assertEqual(p.returncode, 1)
        self.assertIn("car argument, 1, is an integer but should be a pair", p.stderr)
        self.assertIn("song.xtm:4", p.stderr)

    def test_a_file_alone_is_loaded_whole(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            song = Path(d) / "song.xtm"
            song.write_text("(define from-the-file 17)\n(println (* 2 from-the-file))\n")
            p = self.run_client("send", "--file", str(song))
            self.assertEqual((p.returncode, p.stdout.splitlines()[0]), (0, "34"))
            missing = self.run_client("send", "--file", str(Path(d) / "nope.xtm"))
        self.assertEqual(missing.returncode, 2)
        self.assertIn("can't read", missing.stderr)

    def test_a_named_process(self) -> None:
        p = self.run_client("send", "--process", "utility", "(ipc:get-process-name)")
        self.assertEqual((p.returncode, p.stdout), (0, '"utility"\n'))
        p = self.run_client("send", "--process", "nope", "1")
        self.assertEqual(p.returncode, 2)
        self.assertIn("no process named nope", p.stderr)

    def test_no_server_is_status_2(self) -> None:
        p = self.run_client("send", "--port", "1", "(+ 1 2)", port=False)
        self.assertEqual(p.returncode, 2)
        self.assertIn("can't reach an nREPL server at 127.0.0.1:1", p.stderr)

    def test_the_port_is_found_without_being_given(self) -> None:
        # the session wrote .nrepl-port in its working directory: found from
        # there, from a directory below it, and from beside the file sent
        below = self.xtm.cwd / "songs" / "deep"
        below.mkdir(parents=True, exist_ok=True)
        (below / "a.xtm").write_text("(+ 1 2)\n")
        for args, cwd in ((("send", "(+ 1 2)"), self.xtm.cwd), (("send", "(+ 1 2)"), below),
                          (("send", "--file", str(below / "a.xtm"), "--line", "1"), Path("/"))):
            p = self.run_client(*args, cwd=cwd, port=False)
            self.assertEqual((p.returncode, p.stdout), (0, "3\n"), p.stderr)
        # the environment names it where there is no file to find
        env = {"EXTEMPORE_NREPL_PORT": str(self.xtm.port)}
        p = self.run_client("send", "(+ 1 2)", cwd=Path("/"), env=env, port=False)
        self.assertEqual((p.returncode, p.stdout), (0, "3\n"), p.stderr)
        # and an explicit port beats both
        p = self.run_client("send", "--port", "1", "(+ 1 2)", cwd=self.xtm.cwd, env=env,
                            port=False)
        self.assertEqual(p.returncode, 2)


class Status(Clients):
    def test_says_what_is_running(self) -> None:
        p = self.run_client("status")
        self.assertEqual(p.returncode, 0)
        self.assertRegex(p.stdout, rf"^extempore \S+ at 127\.0\.0\.1:{self.xtm.port}\n")
        self.assertRegex(p.stdout, r"processes:.* primary")
        self.assertIn("\n  eval ", p.stdout)

    def test_no_server_is_status_2(self) -> None:
        self.assertEqual(self.run_client("status", "--port", "1", port=False).returncode, 2)


@unittest.skipIf(sys.platform == "win32", "the REPL needs a POSIX terminal")
class Repl(Clients):
    def test_piped_input_runs_form_by_form(self) -> None:
        p = self.run_client("repl", stdin="(+ 1 2)\n(define (sq x)\n  (* x x))\n(sq 9)\n")
        self.assertEqual((p.returncode, p.stdout), (0, "3\nsq\n81\n"))
        p = self.run_client("repl", stdin="(car 1)\n(+ 1 2)\n")
        self.assertEqual(p.returncode, 1)
        self.assertEqual(p.stdout, "3\n")
        self.assertIn("car argument", p.stderr)

    def test_ctrl_c_interrupts_the_running_eval(self) -> None:
        import fcntl
        import pty
        import struct
        import termios

        # the REPL on a terminal of its own, 80 columns wide
        master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))
        repl = subprocess.Popen(
            [str(BIN), "repl", "--port", str(self.xtm.port)],
            stdin=slave, stdout=slave, stderr=slave, start_new_session=True,
            preexec_fn=lambda: fcntl.ioctl(0, termios.TIOCSCTTY, 0),
        )
        os.close(slave)
        seen = b""

        def wait_for(text: bytes, timeout: float = 30) -> None:
            """Read until text has appeared, and forget everything up to it, so
            that the next wait is for something printed after it."""
            nonlocal seen
            deadline = time.monotonic() + timeout
            while text not in seen:
                remaining = deadline - time.monotonic()
                if remaining <= 0 or not select.select([master], [], [], remaining)[0]:
                    self.fail(f"never saw {text!r} in {seen!r}")
                try:
                    seen += os.read(master, 4096)
                except OSError:
                    self.fail(f"the REPL exited; it had printed {seen!r}")
            seen = seen.split(text, 1)[1]

        try:
            wait_for(b"xtm> ")
            os.write(master, b"(let loop ((i 0)) (loop (+ i 1)))\r")
            wait_for(b"(loop (+ i 1)))")
            wait_for(b"\n")  # the line is entered: the eval is on its way
            time.sleep(1.0)
            os.write(master, b"\x03")
            wait_for(b"interrupted")
            wait_for(b"xtm> ")  # the prompt after it, which is when it reads again
            os.write(master, b"(+ 40 2)\r")
            wait_for(b"(+ 40 2)")
            wait_for(b"\n42")
            wait_for(b"xtm> ")  # (what is typed before the prompt is dropped)
            os.write(master, b"\x04")  # Ctrl-D leaves
            self.assertEqual(repl.wait(timeout=30), 0)
        finally:
            if repl.poll() is None:
                repl.kill()
                repl.wait()
            os.close(master)
        # and the session is still there
        self.assertEqual(self.run_client("send", "(+ 1 2)").stdout, "3\n")


class SessionRepl(unittest.TestCase):
    """--repl is that REPL in the session's own terminal, on its own server."""

    def test_no_legacy_server_and_a_piped_repl(self) -> None:
        xtm = Extempore("--no-legacy-server", ready=None, nrepl_port=0)
        try:
            port = int(xtm.wait_for(r"nREPL server started on port (\d+)", 120).group(1))
            xtm.wait_for(r"Ready: connect your editor to nREPL port \d+\n", 120)
            p = subprocess.run([str(BIN), "send", "--port", str(port), "(+ 1 2)"],
                               capture_output=True, text=True, timeout=60)
            self.assertEqual((p.returncode, p.stdout), (0, "3\n"))
        finally:
            xtm.stop()


if __name__ == "__main__":
    unittest.main()

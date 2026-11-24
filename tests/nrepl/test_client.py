"""The command-line client: finding the form at a position, finding the port,
and `eval --file` end to end."""

from __future__ import annotations

import io
import os
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

from harness import Extempore, client

SONG = """\
;; a song
(define tempo 120)   ; "quoted (paren" in a comment

(define (play beat)
  (println "a string with ) and ( in it" #\\( #\\))
  (callback (*metro* (+ beat 1)) 'play (+ beat 1)))

#| a block comment (with parens |#
(bind-func osc:[double,double]*
  (lambda (freq) (* 0.1 (sin freq))))
"""


class FormAt(unittest.TestCase):
    def form(self, line: int, column: int) -> str:
        found = client.form_at(SONG, line, column)
        assert found
        return found[0]

    def test_enclosing_form(self) -> None:
        self.assertTrue(self.form(2, 3).startswith("(define tempo"))
        play = self.form(5, 10)
        self.assertTrue(play.startswith("(define (play beat)"))
        self.assertTrue(play.endswith("'play (+ beat 1)))"))
        self.assertEqual(client.form_at(SONG, 5, 10)[1], 4)

    def test_delimiters_in_strings_comments_and_characters_are_skipped(self) -> None:
        self.assertEqual(self.form(2, 30), "(define tempo 120)")
        self.assertTrue(self.form(6, 1).startswith("(define (play beat)"))

    def test_types_with_brackets(self) -> None:
        self.assertTrue(self.form(10, 5).startswith("(bind-func osc:[double,double]*"))
        self.assertTrue(self.form(10, 5).endswith("(sin freq))))"))

    def test_between_forms_takes_the_one_before(self) -> None:
        self.assertEqual(self.form(3, 1), "(define tempo 120)")
        self.assertTrue(self.form(8, 3).startswith("(define (play beat)"))

    def test_before_any_form_takes_the_first(self) -> None:
        self.assertEqual(self.form(1, 1), "(define tempo 120)")

    def test_unterminated_form_runs_to_the_end(self) -> None:
        self.assertEqual(client.form_at("(a)\n(b (c)", 2, 2), ("(b (c)", 2))

    def test_no_forms(self) -> None:
        self.assertIsNone(client.form_at("; nothing\n", 1, 1))


class FindPort(unittest.TestCase):
    def test_precedence(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            root = Path(d)
            (root / "songs").mkdir()
            song = root / "songs" / "a.xtm"
            song.write_text("(+ 1 2)")
            old = os.environ.pop("EXTEMPORE_NREPL_PORT", None)
            try:
                self.assertEqual(client.find_port(None, song), client.DEFAULT_PORT)
                (root / ".nrepl-port").write_text("4321\n")
                self.assertEqual(client.find_port(None, song), 4321)
                os.environ["EXTEMPORE_NREPL_PORT"] = "5555"
                self.assertEqual(client.find_port(None, song), 5555)
                self.assertEqual(client.find_port(6666, song), 6666)
            finally:
                os.environ.pop("EXTEMPORE_NREPL_PORT", None)
                if old is not None:
                    os.environ["EXTEMPORE_NREPL_PORT"] = old


class Bencode(unittest.TestCase):
    def test_round_trip(self) -> None:
        msg = {"op": "eval", "id": 3, "status": ["done"], "nested": {"b": "x", "a": []}}
        data = client.bencode(msg)
        self.assertEqual(client.bdecode(data), (msg, len(data)))

    def test_incomplete(self) -> None:
        data = client.bencode({"code": "(+ 1 2)"})
        with self.assertRaises(client.Incomplete):
            client.bdecode(data[:-1])


class Cli(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.xtm = Extempore()

    @classmethod
    def tearDownClass(cls) -> None:
        cls.xtm.stop()

    def run_cli(self, *args: str) -> tuple[int, str]:
        buf = io.StringIO()
        with redirect_stdout(buf):
            rc = client.main(["--port", str(self.xtm.port), *args])
        return rc, buf.getvalue()

    def test_eval_the_form_at_a_position(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            song = Path(d) / "song.xtm"
            song.write_text('(define x 1)\n\n(begin\n  (println "hi")\n  (+ 40 2))\n')
            rc, text = self.run_cli(
                "eval", "--editor", "--file", str(song), "--line", "4", "--column", "5"
            )
        self.assertEqual(rc, 0)
        self.assertEqual(text, '"hi"\n=> 42\n')

    def test_errors_carry_the_files_line(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            song = Path(d) / "song.xtm"
            song.write_text("(define x 1)\n\n\n(car x)\n")
            rc, text = self.run_cli(
                "eval", "--editor", "--file", str(song), "--line", "4", "--column", "2"
            )
        self.assertEqual(rc, 0)
        self.assertIn("x is unbound in (car x)", text)
        self.assertIn("song.xtm, line 4", text)

    def test_exit_status_without_editor(self) -> None:
        self.assertEqual(self.run_cli("eval", "(+ 1 2)"), (0, "3\n"))
        rc, _ = self.run_cli("eval", "(car 1)")
        self.assertEqual(rc, 1)

    def test_unreachable_server(self) -> None:
        buf = io.StringIO()
        with redirect_stdout(buf):
            rc = client.main(["--port", "1", "eval", "--editor", "(+ 1 2)"])
        self.assertEqual(rc, 0)
        self.assertIn("can't reach extempore", buf.getvalue())


if __name__ == "__main__":
    unittest.main()

"""extempore lsp, spoken to as an editor speaks to it: JSON-RPC on its stdin
and stdout, with and without an extempore for it to evaluate in."""

from __future__ import annotations

import json
import queue
import subprocess
import sys
import threading
import time
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "nrepl"))
from harness import BIN, Extempore, _free  # noqa: E402

URI = "file:///tmp/extempore-lsp-test/song.xtm"


class Editor:
    """A language client: starts `extempore lsp` and exchanges messages."""

    def __init__(self, *args: str, options: dict | None = None, utf8: bool = False):
        self.proc = subprocess.Popen(
            [str(BIN), "lsp", *args], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        self.inbox: queue.Queue[dict] = queue.Queue()
        self.kept: list[dict] = []
        self.ids = 0
        self.reader = threading.Thread(target=self._read, daemon=True)
        self.reader.start()
        capabilities = {"general": {"positionEncodings": ["utf-8", "utf-16"]}} if utf8 else {}
        self.capabilities = self.request(
            "initialize",
            {"processId": None, "rootUri": None, "capabilities": capabilities,
             "initializationOptions": options or {}},
        )["capabilities"]
        self.notify("initialized", {})

    def _read(self) -> None:
        out = self.proc.stdout
        assert out
        while True:
            length = 0
            while True:
                line = out.readline()
                if not line:
                    return
                if line in (b"\r\n", b"\n"):
                    break
                if line.lower().startswith(b"content-length:"):
                    length = int(line.split(b":")[1])
            self.inbox.put(json.loads(out.read(length)))

    def _send(self, message: dict) -> None:
        body = json.dumps({"jsonrpc": "2.0", **message}).encode()
        assert self.proc.stdin
        self.proc.stdin.write(b"Content-Length: %d\r\n\r\n%s" % (len(body), body))
        self.proc.stdin.flush()

    def notify(self, method: str, params: dict) -> None:
        self._send({"method": method, "params": params})

    def _next(self, wanted, timeout: float) -> dict:
        """The first message wanted() accepts, among those kept and to come."""
        for i, message in enumerate(self.kept):
            if wanted(message):
                return self.kept.pop(i)
        deadline = time.monotonic() + timeout
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise TimeoutError(f"nothing wanted arrived; kept: {self.kept}")
            try:
                message = self.inbox.get(timeout=remaining)
            except queue.Empty:
                continue
            if wanted(message):
                return message
            self.kept.append(message)

    def request(self, method: str, params: dict, timeout: float = 30):
        self.ids += 1
        ident = self.ids
        self._send({"id": ident, "method": method, "params": params})
        reply = self._next(lambda m: m.get("id") == ident and "method" not in m, timeout)
        if "error" in reply:
            raise RuntimeError(reply["error"])
        return reply["result"]

    def notification(self, method: str, timeout: float = 30) -> dict:
        return self._next(lambda m: m.get("method") == method, timeout)["params"]

    def no_notification(self, method: str, wait: float = 0.4) -> None:
        try:
            got = self.notification(method, wait)
        except TimeoutError:
            return
        raise AssertionError(f"unexpected {method}: {got}")

    # --- what an editor does

    def open(self, text: str, uri: str = URI) -> None:
        self.version = 1
        self.notify("textDocument/didOpen", {"textDocument": {
            "uri": uri, "languageId": "extempore", "version": 1, "text": text}})

    def change(self, text: str, uri: str = URI) -> None:
        self.version += 1
        self.notify("textDocument/didChange", {
            "textDocument": {"uri": uri, "version": self.version},
            "contentChanges": [{"text": text}]})

    def save(self, uri: str = URI) -> None:
        self.notify("textDocument/didSave", {"textDocument": {"uri": uri}})

    def diagnostics(self, timeout: float = 30) -> list[dict]:
        return self.notification("textDocument/publishDiagnostics", timeout)["diagnostics"]

    def eval_at(self, line: int, character: int = 0, end: tuple[int, int] | None = None,
                uri: str = URI) -> None:
        stop = end or (line, character)
        self.request("workspace/executeCommand", {"command": "extempore.eval", "arguments": [{
            "uri": uri,
            "range": {"start": {"line": line, "character": character},
                      "end": {"line": stop[0], "character": stop[1]}}}]})

    def message(self, timeout: float = 60) -> tuple[int, str]:
        shown = self.notification("window/showMessage", timeout)
        return shown["type"], shown["message"]

    def stop(self) -> None:
        try:
            self.request("shutdown", {}, timeout=10)
            self.notify("exit", {})
            self.proc.wait(timeout=10)
        finally:
            if self.proc.poll() is None:
                self.proc.kill()
                self.proc.wait()
            for stream in (self.proc.stdin, self.proc.stdout, self.proc.stderr):
                if stream:
                    stream.close()


ERROR, WARNING, INFO = 1, 2, 3

SONG = """\
;; a song
(define tempo 120)

(define (play beat)
  (println "beat" beat)
  (* beat 2))

(bind-func osc:[double,double]*
  (lambda (freq)
    (* 0.1 (sin freq))))
"""


class WithoutExtempore(unittest.TestCase):
    """What a parser can say, and what it says when nothing is running."""

    def setUp(self) -> None:
        self.editor = Editor("--port", "1")
        self.addCleanup(self.editor.stop)

    def test_capabilities(self) -> None:
        caps = self.editor.capabilities
        self.assertEqual(caps["positionEncoding"], "utf-16")
        self.assertEqual(set(caps["executeCommandProvider"]["commands"]),
                         {"extempore.eval", "extempore.evalBuffer", "extempore.interrupt"})
        self.assertTrue(caps["documentSymbolProvider"])
        self.assertTrue(caps["codeActionProvider"])
        self.assertTrue(caps["hoverProvider"])
        self.assertIn("completionProvider", caps)
        utf8 = Editor(utf8=True)
        self.addCleanup(utf8.stop)
        self.assertEqual(utf8.capabilities["positionEncoding"], "utf-8")

    def test_reader_problems_when_a_file_is_opened(self) -> None:
        self.editor.open("(define x 1))\n(define (f y)\n  (+ y 1\n")
        found = self.editor.diagnostics()
        self.assertEqual(len(found), 2)
        self.assertEqual(found[0]["range"]["start"], {"line": 0, "character": 12})
        self.assertIn("nothing to close", found[0]["message"])
        self.assertEqual(found[1]["range"]["start"], {"line": 1, "character": 0})
        self.assertIn("never ends", found[1]["message"])
        # they follow the edits which fix them
        self.editor.change("(define x 1)\n(define (f y)\n  (+ y 1\n")
        self.assertEqual(len(self.editor.diagnostics()), 1)
        self.editor.change("(define x 1)\n(define (f y)\n  (+ y 1))\n")
        self.assertEqual(self.editor.diagnostics(), [])

    def test_a_form_being_typed_is_not_a_problem_until_saved(self) -> None:
        self.editor.open("(define x 1)\n")
        self.assertEqual(self.editor.diagnostics(), [])
        self.editor.change("(define x 1)\n(define (f y)\n  (+ y")
        self.assertEqual(self.editor.diagnostics(), [])
        self.editor.save()
        self.assertEqual(len(self.editor.diagnostics()), 1)

    def test_outline_and_folding(self) -> None:
        self.editor.open(SONG)
        symbols = self.editor.request("textDocument/documentSymbol",
                                      {"textDocument": {"uri": URI}})
        self.assertEqual([(s["name"], s["detail"], s["kind"]) for s in symbols],
                         [("tempo", "define", 13), ("play", "define", 12),
                          ("osc", "bind-func", 12)])
        self.assertEqual(symbols[2]["selectionRange"],
                         {"start": {"line": 7, "character": 11},
                          "end": {"line": 7, "character": 14}})
        folds = self.editor.request("textDocument/foldingRange", {"textDocument": {"uri": URI}})
        self.assertEqual(folds, [{"startLine": 3, "endLine": 5}, {"startLine": 7, "endLine": 9}])

    def test_code_actions(self) -> None:
        self.editor.open(SONG)

        def actions(start: tuple[int, int], end: tuple[int, int]) -> list[dict]:
            return self.editor.request("textDocument/codeAction", {
                "textDocument": {"uri": URI}, "context": {"diagnostics": []},
                "range": {"start": {"line": start[0], "character": start[1]},
                          "end": {"line": end[0], "character": end[1]}}})

        at_cursor = actions((4, 3), (4, 3))
        self.assertEqual([a["title"] for a in at_cursor],
                         ["Evaluate top-level form", "Evaluate buffer"])
        self.assertEqual(at_cursor[0]["command"]["command"], "extempore.eval")
        self.assertEqual(at_cursor[0]["command"]["arguments"][0]["uri"], URI)
        self.assertEqual(actions((4, 2), (4, 23))[0]["title"], "Evaluate selection")
        # one character is a cursor too, in an editor whose cursor is a selection
        self.assertEqual(actions((4, 2), (4, 3))[0]["title"], "Evaluate top-level form")

    def test_eval_says_there_is_nothing_to_talk_to(self) -> None:
        self.editor.open(SONG)
        self.editor.diagnostics()
        self.editor.eval_at(1, 3)
        kind, text = self.editor.message()
        self.assertEqual(kind, ERROR)
        self.assertIn("no nREPL server at 127.0.0.1:1", text)

    def test_completion_and_hover_have_nothing_to_say(self) -> None:
        self.editor.open("(bind-fu\n")
        where = {"textDocument": {"uri": URI}, "position": {"line": 0, "character": 8}}
        self.assertEqual(self.editor.request("textDocument/completion", where), [])
        self.assertIsNone(self.editor.request("textDocument/hover", where))
        self.editor.no_notification("window/showMessage")  # and no complaint about it

    def test_unknown_requests_get_an_error(self) -> None:
        with self.assertRaises(RuntimeError):
            self.editor.request("textDocument/formatting", {"textDocument": {"uri": URI}})


class WithExtempore(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.xtm = Extempore()

    @classmethod
    def tearDownClass(cls) -> None:
        cls.xtm.stop()

    def setUp(self) -> None:
        self.editor = Editor(options={"port": self.xtm.port})
        self.addCleanup(self.editor.stop)

    def open(self, text: str) -> None:
        self.editor.open(text)
        self.assertEqual(self.editor.diagnostics(), [])

    def test_eval_the_form_at_the_cursor(self) -> None:
        self.open(SONG)
        self.editor.eval_at(1, 4)
        self.assertEqual(self.editor.message(), (INFO, "=> 120"))
        self.editor.eval_at(4, 9)  # inside play
        self.assertEqual(self.editor.message(), (INFO, "=> play"))
        self.assertEqual(self.editor.diagnostics(), [])

    def test_output_goes_to_the_log_and_its_first_line_to_the_status(self) -> None:
        self.open('(begin (println "one") (println "two") (+ 40 2))\n')
        self.editor.eval_at(0, 0)
        self.assertEqual(self.editor.message(), (INFO, '"one"  => 42'))
        self.assertEqual(self.editor.notification("window/logMessage")["message"],
                         '"one"\n"two"\n')

    def test_the_buffer_is_evaluated_as_it_stands_unsaved(self) -> None:
        self.open("(+ 1 2)\n")
        self.editor.change("(+ 1 2)\n(* 6 7)\n")
        self.editor.diagnostics()
        self.editor.eval_at(1, 2)
        self.assertEqual(self.editor.message(), (INFO, "=> 42"))

    def test_a_selection(self) -> None:
        self.open("(list (+ 1 2) (* 6 7))\n")
        self.editor.eval_at(0, 14, end=(0, 21))
        self.assertEqual(self.editor.message(), (INFO, "=> 42"))
        # a selection of one character is a cursor: the form it is in
        self.editor.eval_at(0, 14, end=(0, 15))
        self.assertEqual(self.editor.message(), (INFO, "=> (3 42)"))

    def test_positions_are_in_utf16_units(self) -> None:
        # The astral character is one character, two UTF-16 units and four
        # bytes. In UTF-16 units the second form opens at 21; counted in
        # characters 20 would be its parenthesis, and in bytes 21 would still
        # be inside the first form.
        self.open('(string-length "😀") (+ 1 2)\n')
        self.editor.eval_at(0, 21)
        self.assertEqual(self.editor.message(), (INFO, "=> 3"))
        self.editor.eval_at(0, 20)
        self.assertEqual(self.editor.message(), (INFO, "=> 4"))

    def test_an_error_marks_its_form_until_it_is_fixed(self) -> None:
        text = "(define ok 1)\n\n(define (broken)\n  (car 5))\n(broken)\n"
        self.open(text)
        self.editor.eval_at(3, 3)
        self.assertEqual(self.editor.message(), (INFO, "=> broken"))
        self.editor.diagnostics()
        self.editor.eval_at(4, 1)
        kind, shown = self.editor.message()
        self.assertEqual(kind, ERROR)
        self.assertIn("car argument, 5, is an integer", shown)
        (found,) = self.editor.diagnostics()
        self.assertEqual(found["range"], {"start": {"line": 4, "character": 0},
                                          "end": {"line": 4, "character": 8}})
        self.assertIn("car argument, 5, is an integer", found["message"])
        self.assertEqual(found["source"], "extempore")

        # a line added above: the mark moves with the form
        self.editor.change(";; a comment\n" + text)
        (moved,) = self.editor.diagnostics()
        self.assertEqual(moved["range"]["start"], {"line": 5, "character": 0})

        # the cause is fixed and the form evaluated again: the mark goes
        fixed = (";; a comment\n" + text).replace("(car 5)", "(car '(5))")
        self.editor.change(fixed)
        self.assertEqual(len(self.editor.diagnostics()), 1)  # (broken) itself is unchanged
        self.editor.eval_at(4, 3)
        self.editor.message()
        self.editor.diagnostics()
        self.editor.eval_at(5, 1)
        self.assertEqual(self.editor.message(), (INFO, "=> 5"))
        self.assertEqual(self.editor.diagnostics(), [])

    def test_editing_a_marked_form_clears_its_mark(self) -> None:
        self.open("(car 5)\n")
        self.editor.eval_at(0, 0)
        self.editor.message()
        self.assertEqual(len(self.editor.diagnostics()), 1)
        self.editor.change("(car '(5 6))\n")
        self.assertEqual(self.editor.diagnostics(), [])

    def test_an_xtlang_error_is_reported_the_same_way(self) -> None:
        self.open('(bind-func lsp_bad (lambda (x:i64) (+ x "s")))\n')
        self.editor.eval_at(0, 5)
        kind, _ = self.editor.message(timeout=120)
        self.assertEqual(kind, ERROR)
        (found,) = self.editor.diagnostics()
        self.assertEqual(found["range"]["start"], {"line": 0, "character": 0})
        self.assertIn("Type Error", found["message"])

    def test_eval_buffer(self) -> None:
        self.open('(define lsp-a 20)\n(println "loading")\n(+ lsp-a 22)\n')
        self.editor.request("workspace/executeCommand", {
            "command": "extempore.evalBuffer", "arguments": [{"uri": URI}]})
        self.assertEqual(self.editor.message(), (INFO, '"loading"  => 42'))
        self.assertEqual(self.editor.diagnostics(), [])
        # a form which fails is marked, wherever the cursor was
        self.editor.change("(define lsp-a 20)\n\n(car lsp-a)\n(+ 1 2)\n")
        self.editor.diagnostics()
        self.editor.request("workspace/executeCommand", {
            "command": "extempore.evalBuffer", "arguments": [{"uri": URI}]})
        self.assertEqual(self.editor.message()[0], ERROR)
        (found,) = self.editor.diagnostics()
        self.assertEqual(found["range"]["start"], {"line": 2, "character": 0})

    def test_interrupt(self) -> None:
        self.open("(let loop ((i 0)) (set! i (+ i 1)) (loop i))\n")
        self.editor.eval_at(0, 0)
        time.sleep(0.5)
        self.editor.request("workspace/executeCommand",
                            {"command": "extempore.interrupt", "arguments": []})
        self.assertEqual(self.editor.message(), (WARNING, "interrupted"))
        # and with nothing running it says so
        self.editor.request("workspace/executeCommand",
                            {"command": "extempore.interrupt", "arguments": []})
        self.assertEqual(self.editor.message(), (INFO, "nothing is being evaluated"))

    def test_completion(self) -> None:
        self.open("(define lsp-complete-one 1)\n(define (lsp-complete-two x) x)\n")
        self.editor.request("workspace/executeCommand", {
            "command": "extempore.evalBuffer", "arguments": [{"uri": URI}]})
        self.editor.message()
        self.editor.change("(define lsp-complete-one 1)\n(+ 1 lsp-compl\n")
        items = self.editor.request("textDocument/completion", {
            "textDocument": {"uri": URI}, "position": {"line": 1, "character": 14}})
        self.assertEqual([(i["label"], i["kind"]) for i in items],
                         [("lsp-complete-one", 6), ("lsp-complete-two", 3)])
        # the edit replaces the whole of what was typed, hyphens and all
        self.assertEqual(items[0]["textEdit"], {
            "newText": "lsp-complete-one",
            "range": {"start": {"line": 1, "character": 5},
                      "end": {"line": 1, "character": 14}}})
        # xtlang's own names complete too
        self.editor.change("(bind-fu\n")
        items = self.editor.request("textDocument/completion", {
            "textDocument": {"uri": URI}, "position": {"line": 0, "character": 8}})
        self.assertIn(("bind-func", 14), [(i["label"], i["kind"]) for i in items])

    def test_hover(self) -> None:
        self.open('(bind-func lsp_hover_fn "halves its argument"\n'
                  "  (lambda (x:double) (/ x 2.0)))\n(lsp_hover_fn 3.0)\n"
                  "(bind-func lsp_hover_typed:[i64,i64]* (lambda (n) (+ n 1)))\n")
        self.editor.request("workspace/executeCommand", {
            "command": "extempore.evalBuffer", "arguments": [{"uri": URI}]})
        self.assertEqual(self.editor.message(timeout=120)[0], INFO)

        def hover(line: int, character: int):
            return self.editor.request("textDocument/hover", {
                "textDocument": {"uri": URI}, "position": {"line": line, "character": character}})

        shown = hover(2, 5)
        self.assertEqual(shown["range"], {"start": {"line": 2, "character": 1},
                                          "end": {"line": 2, "character": 13}})
        self.assertEqual(shown["contents"]["kind"], "markdown")
        text = shown["contents"]["value"]
        self.assertIn("(lsp_hover_fn x)  ; [double,double]*", text)
        self.assertIn("halves its argument", text)
        # a name with its type annotation is looked up by the name
        self.assertIn("(lsp_hover_typed n)  ; [i64,i64]*", hover(3, 15)["contents"]["value"])
        # a name with a colon of its own is looked up whole
        self.editor.change("(sys:load \"x\")\n")
        self.assertIn("sys:load", hover(0, 4)["contents"]["value"])
        self.assertIsNone(hover(0, 12))  # a string is nothing

    def test_a_named_process(self) -> None:
        editor = Editor(options={"port": self.xtm.port, "process": "utility"})
        self.addCleanup(editor.stop)
        editor.open("(ipc:get-process-name)\n")
        editor.eval_at(0, 0)
        self.assertEqual(editor.message(), (INFO, '=> "utility"'))


class Reconnect(unittest.TestCase):
    def test_it_finds_extempore_again_after_a_restart(self) -> None:
        port = next(p for p in range(47000, 47999) if _free(p))
        xtm = Extempore(nrepl_port=port)
        editor = Editor(options={"port": port})
        self.addCleanup(editor.stop)
        try:
            editor.open("(+ 1 2)\n")
            editor.eval_at(0, 0)
            self.assertEqual(editor.message(), (INFO, "=> 3"))
        finally:
            xtm.stop()
        editor.eval_at(0, 0)
        self.assertEqual(editor.message()[0], ERROR)
        xtm = Extempore(nrepl_port=port)
        try:
            editor.eval_at(0, 0)
            self.assertEqual(editor.message(), (INFO, "=> 3"))
        finally:
            xtm.stop()


if __name__ == "__main__":
    unittest.main()

"""The nREPL server against a running extempore (backlog/docs/doc-002)."""

from __future__ import annotations

import socket
import threading
import time
import unittest

from harness import Extempore, client, out, statuses, values

bencode = client.bencode


class Protocol(unittest.TestCase):
    """One extempore for the lot; each test uses its own connections."""

    @classmethod
    def setUpClass(cls) -> None:
        cls.xtm = Extempore()

    @classmethod
    def tearDownClass(cls) -> None:
        cls.xtm.stop()

    def eval(self, code: str, **extra: object) -> list[dict]:
        with self.xtm.client() as c:
            return c.request(op="eval", code=code, **extra)

    # --- describe and friends -------------------------------------------

    def test_describe_lists_the_ops_and_processes(self) -> None:
        with self.xtm.client() as c:
            (msg,) = c.request(op="describe")
        self.assertEqual(
            set(msg["ops"]),
            {
                "clone",
                "close",
                "completions",
                "describe",
                "eval",
                "interrupt",
                "load-file",
                "lookup",
                "ls-sessions",
            },
        )
        self.assertIn("version-string", msg["versions"]["extempore"])
        self.assertIn("version-string", msg["versions"]["nrepl"])
        self.assertEqual(set(msg["aux"]["extempore/processes"]), {"primary", "utility"})
        self.assertEqual(msg["status"], ["done"])

    def test_unknown_op(self) -> None:
        with self.xtm.client() as c:
            (msg,) = c.request(op="stdin", stdin="x")
        self.assertEqual(msg["status"], ["error", "unknown-op", "done"])
        self.assertEqual(msg["op"], "stdin")
        self.assertEqual(msg["id"], "c1")

    def test_eval_without_code(self) -> None:
        with self.xtm.client() as c:
            (msg,) = c.request(op="eval")
        self.assertEqual(msg["status"], ["error", "no-code", "done"])

    # --- eval -----------------------------------------------------------

    def test_one_value_per_form_then_one_done(self) -> None:
        rs = self.eval('(+ 1 2) (* 2 3)\n(string-append "a" "b")')
        self.assertEqual(values(rs), ["3", "6", '"ab"'])
        self.assertEqual(statuses(rs), ["done"])
        self.assertEqual(rs[-1]["status"], ["done"])
        for r in rs:
            self.assertEqual(r["id"], "c1")
            self.assertIn("session", r)

    def test_eval_stops_at_the_first_error(self) -> None:
        rs = self.eval("(+ 1 2) (car 5) (+ 3 4)")
        self.assertEqual(values(rs), ["3"])
        self.assertEqual(statuses(rs), ["eval-error", "done"])
        err = "".join(r["err"] for r in rs if "err" in r)
        self.assertIn("car argument, 5, is an integer but should be a pair", err)
        (error,) = [r for r in rs if "ex" in r]
        self.assertEqual(error["ex"], "wrong-type-arg")
        self.assertEqual(error["root-ex"], "wrong-type-arg")
        info = error["extempore/error"]
        self.assertEqual(info["kind"], "wrong-type-arg")
        self.assertEqual(info["form"], "(car 5)")
        self.assertEqual(info["line"], 1)

    def test_out_arrives_before_done_without_colour_codes(self) -> None:
        rs = self.eval(
            '(println \'hello) (print-with-colors \'red \'default #t (print "in red")) (print-error "oops") 42'
        )
        text = out(rs)
        self.assertIn("hello\n", text)
        self.assertIn("in red", text)
        self.assertIn("ERROR: oops\n", text)
        self.assertNotIn("\x1b", text)
        self.assertEqual(values(rs)[-1], "42")
        last_out = max(i for i, r in enumerate(rs) if "out" in r)
        self.assertLess(last_out, len(rs) - 1)
        # the console still shows it
        self.xtm.wait_for("hello")

    def test_xtlang_compile_error(self) -> None:
        rs = self.eval("(bind-func nrepl_bad_fn (lambda (x:i64) (+ x 1.0)))")
        (error,) = [r for r in rs if "ex" in r]
        self.assertEqual(error["ex"], "xtlang-type-error")
        self.assertEqual(error["status"], ["eval-error"])
        err = "".join(r["err"] for r in rs if "err" in r)
        self.assertIn("no overload matches the argument types [i64,double]", err)
        self.assertIn("couldn't resolve type: nrepl_bad_fn", err)
        self.assertNotIn("\x1b", err)
        # the report reaches the client once, as err, not as out too
        self.assertNotIn("couldn't resolve type", out(rs))
        self.xtm.wait_for("couldn't resolve type: nrepl_bad_fn")

    def test_xtlang_compiles_and_runs(self) -> None:
        rs = self.eval(
            "(bind-func nrepl_twice (lambda (x:i64) (* x 2))) (nrepl_twice 21)"
        )
        self.assertEqual(values(rs)[-1], "42")
        self.assertIn("Compiled:", out(rs))

    def test_an_eval_after_errors_succeeds(self) -> None:
        for _ in range(3):
            self.eval("(car 5)")
        rs = self.eval("(+ 1 1)")
        self.assertEqual(values(rs), ["2"])
        self.assertEqual(statuses(rs), ["done"])

    def test_file_and_line_place_errors(self) -> None:
        rs = self.eval("\n; a comment\n\n  (car 5)", file="/music/song.xtm", line=10)
        (error,) = [r for r in rs if "ex" in r]
        self.assertEqual(error["extempore/error"]["line"], 13)
        self.assertEqual(error["extempore/error"]["file"], "/music/song.xtm")
        err = "".join(r["err"] for r in rs if "err" in r)
        self.assertIn("/music/song.xtm, line 13", err)

    def test_read_error(self) -> None:
        rs = self.eval("(+ 1 2)\n\n(car (list 1)", file="song.xtm", line=1)
        self.assertEqual(values(rs), ["3"])
        (error,) = [r for r in rs if "ex" in r]
        self.assertEqual(error["extempore/error"]["line"], 3)
        self.assertEqual(statuses(rs), ["eval-error", "done"])

    def test_non_utf8_code(self) -> None:
        rs = self.eval(b'(string-length "\xff\xfe")')  # type: ignore[arg-type]
        self.assertEqual(values(rs), ["2"])

    def test_load_file_sends_the_last_value(self) -> None:
        with self.xtm.client() as c:
            rs = c.request(
                op="load-file",
                file="(define nrepl-lf 1)\n(+ nrepl-lf 41)\n",
                **{"file-path": "lib/thing.xtm"},
            )
        self.assertEqual(values(rs), ["42"])
        with self.xtm.client() as c:
            rs = c.request(
                op="load-file",
                file="(+ 1 2)\n\n(car 5)\n",
                **{"file-path": "lib/thing.xtm"},
            )
        err = "".join(r["err"] for r in rs if "err" in r)
        self.assertIn("lib/thing.xtm, line 3", err)
        self.assertEqual(values(rs), [])

    # --- sessions -------------------------------------------------------

    def test_sessions(self) -> None:
        with self.xtm.client() as c:
            (cloned,) = c.request(op="clone")
            session = cloned["new-session"]
            (listed,) = c.request(op="ls-sessions")
            self.assertIn(session, listed["sessions"])
            rs = c.request(op="eval", code="(+ 2 2)", session=session)
            self.assertEqual(values(rs), ["4"])
            self.assertTrue(all(r["session"] == session for r in rs))
            (closed,) = c.request(op="close", session=session)
            self.assertEqual(closed["status"], ["session-closed", "done"])
            (gone,) = c.request(op="eval", code="1", session=session)
            self.assertEqual(gone["status"], ["error", "unknown-session", "done"])

    def test_sessions_close_with_their_connection_but_definitions_stay(self) -> None:
        with self.xtm.client() as c:
            session = c.request(op="clone")[0]["new-session"]
            c.request(op="eval", code="(define nrepl-kept 99)", session=session)
        time.sleep(0.5)  # the server notices the disconnect on its next poll
        with self.xtm.client() as c:
            (listed,) = c.request(op="ls-sessions")
            self.assertNotIn(session, listed["sessions"])
            (gone,) = c.request(op="eval", code="1", session=session)
            self.assertIn("unknown-session", gone["status"])
            self.assertEqual(values(c.request(op="eval", code="nrepl-kept")), ["99"])

    def test_utility_session_runs_while_primary_is_busy(self) -> None:
        busy_loop = "(let loop ((i 0)) (if (< i 10000000) (begin (set! i (+ i 1)) (loop i)) i))"
        with self.xtm.client() as busy, self.xtm.client() as c:
            busy_id = busy.send(op="eval", code=busy_loop)
            session = c.request(op="clone", **{"extempore/process": "utility"})[0]["new-session"]
            rs = c.request(op="eval", code="(ipc:get-process-name)", session=session)
            utility_done = time.monotonic()
            self.assertEqual(values(rs), ['"utility"'])
            self.assertEqual(values(list(busy.responses(busy_id))), ["10000000"])
            self.assertLess(utility_done + 0.1, time.monotonic())

    def test_clone_unknown_process(self) -> None:
        with self.xtm.client() as c:
            (msg,) = c.request(op="clone", **{"extempore/process": "nonesuch"})
        self.assertEqual(msg["status"], ["error", "unknown-process", "done"])

    def test_replies_go_only_to_the_requester(self) -> None:
        results: dict[str, list[dict]] = {}

        def run(name: str) -> None:
            with self.xtm.client() as c:
                results[name] = c.request(
                    op="eval", code=f'(dotimes (i 50) (println "{name}" i)) "{name}"'
                )

        threads = [
            threading.Thread(target=run, args=(n,)) for n in ("alpha", "beta", "gamma")
        ]
        for t in threads:
            t.start()
        for t in threads:
            t.join()
        for name, rs in results.items():
            text = out(rs)
            self.assertEqual(text.count(name), 50, name)
            for other in results:
                if other != name:
                    self.assertNotIn(other, text)
            self.assertEqual(values(rs)[-1], f'"{name}"')

    # --- completions and lookup -------------------------------------------

    def candidates(self, c, prefix: str) -> dict[str, str]:
        (reply,) = c.request(op="completions", prefix=prefix)
        self.assertEqual(reply["status"], ["done"])
        return {item["candidate"]: item["type"] for item in reply["completions"]}

    def test_completions_of_scheme_and_xtlang_names(self) -> None:
        with self.xtm.client() as c:
            c.request(op="eval", code="""
                (define nrepl-complete-var 1)
                (define (nrepl-complete-fn x) x)
                (define-macro (nrepl-complete-macro x) x)
                (bind-func nrepl_complete_xt (lambda (x:i64) (* x 2)))
                (bind-type NreplCompleteType <i64,double>)""")
            found = self.candidates(c, "nrepl-complete-")
            self.assertEqual(found, {"nrepl-complete-fn": "function",
                                     "nrepl-complete-macro": "macro",
                                     "nrepl-complete-var": "var"})
            self.assertEqual(list(found), sorted(found))
            self.assertEqual(self.candidates(c, "nrepl_complete_"),
                             {"nrepl_complete_xt": "function"})
            # a type has no Scheme binding: the compiler's caches know it
            self.assertEqual(self.candidates(c, "NreplComplete")["NreplCompleteType"], "class")
            self.assertEqual(self.candidates(c, "bind-fu")["bind-func"], "special-form")
            self.assertEqual(self.candidates(c, "no-such-prefix-anywhere"), {})
            self.assertEqual(self.candidates(c, ""), {})

    def test_completions_are_capped(self) -> None:
        with self.xtm.client() as c:
            self.assertEqual(len(self.candidates(c, "x")), 200)

    def test_lookup(self) -> None:
        with self.xtm.client() as c:
            c.request(op="eval", code="""
                (bind-func nrepl_lookup_xt "doubles its argument" (lambda (x:i64) (* x 2)))
                (define (nrepl-lookup-fn a b) a)""")
            (reply,) = c.request(op="lookup", sym="nrepl_lookup_xt")
            self.assertEqual(reply["status"], ["done"])
            info = reply["info"]
            self.assertEqual(info["name"], "nrepl_lookup_xt")
            self.assertEqual(info["extempore/type"], "[i64,i64]*")
            self.assertEqual(info["extempore/kind"], "closure")
            self.assertEqual(info["arglists-str"], "(x)")
            self.assertEqual(info["doc"], "doubles its argument")
            (reply,) = c.request(op="lookup", sym="nrepl-lookup-fn")
            self.assertEqual(reply["info"]["arglists-str"], "(a b)")
            self.assertEqual(reply["info"]["extempore/kind"], "scheme closure")
            self.assertNotIn("doc", reply["info"])
            (reply,) = c.request(op="lookup", sym="nothing-by-this-name")
            self.assertEqual(reply["status"], ["no-info", "done"])
            self.assertNotIn("info", reply)

    def test_completions_and_lookup_need_their_argument(self) -> None:
        with self.xtm.client() as c:
            (reply,) = c.request(op="completions")
            self.assertEqual(reply["status"], ["error", "no-symbol", "done"])
            (reply,) = c.request(op="lookup")
            self.assertEqual(reply["status"], ["error", "no-symbol", "done"])

    # --- interrupt ------------------------------------------------------

    LONG_LOOP = "(let loop ((i 0)) (set! nrepl-spin i) (loop (+ i 1)))"

    def test_interrupt_a_running_eval(self) -> None:
        with self.xtm.client() as c:
            session = c.request(op="clone")[0]["new-session"]
            c.request(op="eval", code="(define nrepl-spin 0)", session=session)
            eval_id = c.send(op="eval", code=self.LONG_LOOP, session=session)
            time.sleep(0.5)
            (ack,) = c.request(
                op="interrupt", session=session, **{"interrupt-id": eval_id}
            )
            self.assertEqual(ack["status"], ["done"])
            rs = list(c.responses(eval_id))
            self.assertEqual(statuses(rs), ["interrupted", "done"])
            self.assertEqual(
                values(c.request(op="eval", code="(+ 1 1)", session=session)), ["2"]
            )

    def test_interrupt_without_an_id_stops_the_sessions_eval(self) -> None:
        with self.xtm.client() as c:
            session = c.request(op="clone")[0]["new-session"]
            c.request(op="eval", code="(define nrepl-spin 0)", session=session)
            eval_id = c.send(op="eval", code=self.LONG_LOOP, session=session)
            time.sleep(0.5)
            c.request(op="interrupt", session=session)
            self.assertIn("interrupted", statuses(list(c.responses(eval_id))))

    def test_interrupt_replies(self) -> None:
        with self.xtm.client() as c:
            session = c.request(op="clone")[0]["new-session"]
            (idle,) = c.request(op="interrupt", session=session)
            self.assertEqual(idle["status"], ["session-idle", "done"])
            (unknown,) = c.request(op="interrupt", session="no-such-session")
            self.assertEqual(unknown["status"], ["error", "unknown-session", "done"])
            c.request(op="eval", code="(define nrepl-spin 0)", session=session)
            eval_id = c.send(op="eval", code=self.LONG_LOOP, session=session)
            time.sleep(0.5)
            (mismatch,) = c.request(
                op="interrupt", session=session, **{"interrupt-id": "other"}
            )
            self.assertEqual(
                mismatch["status"], ["error", "interrupt-id-mismatch", "done"]
            )
            c.request(op="interrupt", session=session, **{"interrupt-id": eval_id})
            self.assertIn("interrupted", statuses(list(c.responses(eval_id))))

    def test_time_limit(self) -> None:
        with self.xtm.client() as c:
            c.request(
                op="eval", code="(define nrepl-spin 0) (sys:set-default-timeout 44100)"
            )
            try:
                rs = c.request(op="eval", code=self.LONG_LOOP)
            finally:
                c.request(op="eval", code="(sys:set-default-timeout (* 44100 60))")
        self.assertEqual(statuses(rs), ["eval-error", "done"])
        self.assertIn("time limit", "".join(r.get("err", "") for r in rs))

    # --- the wire -------------------------------------------------------

    def raw(self) -> socket.socket:
        return socket.create_connection(("127.0.0.1", self.xtm.port), timeout=30)

    def test_frames_split_across_reads_and_joined_in_one(self) -> None:
        with self.xtm.client() as c:
            msg = bencode({"op": "eval", "code": "(+ 40 2)", "id": "slow"})
            for i in range(len(msg)):
                c.sock.sendall(msg[i : i + 1])
                time.sleep(0.002)
            self.assertEqual(values(list(c.responses("slow"))), ["42"])
            c.sock.sendall(
                bencode({"op": "eval", "code": "1", "id": "a"})
                + bencode({"op": "eval", "code": "2", "id": "b"})
            )
            self.assertEqual(values(list(c.responses("a"))), ["1"])
            self.assertEqual(values(list(c.responses("b"))), ["2"])

    def test_malformed_input_closes_only_that_connection(self) -> None:
        with self.raw() as s:
            s.sendall(b"this is not bencode")
            data = s.recv(65536)
            self.assertIn(b"decode-error", data)
            s.settimeout(10)
            self.assertEqual(s.recv(65536), b"")
        with self.raw() as s:
            s.sendall(b"d4:code1:12:op4:eval99999999999:")
            self.assertIn(b"too large", s.recv(65536))
        self.assertEqual(values(self.eval("(+ 1 1)")), ["2"])

    def test_disconnect_mid_eval(self) -> None:
        with self.xtm.client() as c:
            c.send(
                op="eval",
                code="(let loop ((i 0)) (if (< i 2000000) (begin (println i) (loop (+ i 1)))))",
            )
        self.assertEqual(values(self.eval("(+ 2 3)")), ["5"])

    def test_a_client_that_never_reads_is_dropped(self) -> None:
        s = self.raw()
        s.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 4096)
        s.sendall(
            bencode(
                {
                    "op": "eval",
                    "id": "flood",
                    "code": "(dotimes (i 120000) (println (make-string 200 #\\x)))",
                }
            )
        )
        # the eval runs to its end although the client reads nothing
        deadline = time.monotonic() + 120
        while time.monotonic() < deadline:
            if values(self.eval("(+ 1 1)")) == ["2"]:
                break
        s.settimeout(30)
        received = 0
        try:
            while chunk := s.recv(1 << 20):
                received += len(chunk)
        except OSError:
            pass
        s.close()
        self.assertLess(received, 120000 * 201)  # cut off, not delivered in full

    def test_the_old_eval_server_still_works(self) -> None:
        with socket.create_connection(
            ("127.0.0.1", self.xtm.eval_port), timeout=30
        ) as s:
            banner = s.recv(4096)
            self.assertTrue(banner.endswith(b"\0"))
            s.sendall(b"(+ 1 2)\r\n")
            self.assertEqual(s.recv(4096), b"3\0")


class Startup(unittest.TestCase):
    def test_port_zero_writes_and_removes_nrepl_port(self) -> None:
        with Extempore() as xtm:
            port_file = xtm.cwd / ".nrepl-port"
            self.assertEqual(int(port_file.read_text()), xtm.port)
            with xtm.client() as other, xtm.client() as c:
                c.send(op="eval", code="(quit 0)")
                self.assertEqual(xtm.proc.wait(timeout=30), 0)
                with self.assertRaises((ConnectionError, OSError)):
                    other.request(op="describe")
            self.assertFalse(port_file.exists())

    def test_an_explicit_port_in_use_is_fatal(self) -> None:
        with socket.socket() as blocker:
            blocker.bind(("127.0.0.1", 0))
            blocker.listen()
            port = blocker.getsockname()[1]
            with Extempore(nrepl_port=port, wait_for_banner=False) as xtm:
                self.assertNotEqual(xtm.proc.wait(timeout=60), 0)
                self.assertIn("could not listen", xtm.console_text())

    def test_the_default_port_in_use_is_a_warning(self) -> None:
        with socket.socket() as blocker:
            try:
                blocker.bind(("127.0.0.1", client.DEFAULT_PORT))
            except OSError:
                self.skipTest("the default nREPL port is already in use")
            blocker.listen()
            with Extempore(nrepl_port=None, wait_for_banner=False) as xtm:
                xtm.wait_for("could not listen")
                xtm.wait_for("Listening on TCP port")
                with socket.create_connection(
                    ("127.0.0.1", xtm.eval_port), timeout=60
                ) as s:
                    s.recv(4096)
                    s.sendall(b"(+ 1 2)\r\n")
                    self.assertEqual(s.recv(4096), b"3\0")

    def test_no_nrepl(self) -> None:
        with Extempore("--no-nrepl", nrepl_port=None, wait_for_banner=False) as xtm:
            xtm.wait_for("Listening on TCP port")
            self.assertNotIn("nREPL", xtm.console_text())

    def test_a_request_before_startup_finishes_is_answered_after_it(self) -> None:
        with Extempore(wait_for_banner=True) as xtm:
            with xtm.client() as c:
                # base is still loading, or just has: either way the eval sees it
                rs = c.request(op="eval", code="(defined? '*xtmlib-base-loaded*)")
        self.assertEqual(values(rs), ["#t"])


if __name__ == "__main__":
    unittest.main()

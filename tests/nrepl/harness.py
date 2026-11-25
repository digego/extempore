"""Shared fixtures for the nREPL tests: an extempore process to talk to, and
the tests' own client (bencode_client.py). EXTEMPORE_BIN names the binary
(ctest sets it); the default is build/extempore.
"""

from __future__ import annotations

import os
import random
import re
import socket
import subprocess
import tempfile
import threading
import time
from pathlib import Path

import bencode_client as client

REPO = Path(__file__).resolve().parents[2]
BIN = Path(os.environ.get("EXTEMPORE_BIN", REPO / "build" / "extempore")).resolve()
Client = client.Client


def _free(port: int) -> bool:
    with socket.socket() as s:
        try:
            s.bind(("127.0.0.1", port))
            return True
        except OSError:
            return False


def free_port_pair() -> int:
    """A port P with P - 1 free too, for extempore's primary and utility eval
    servers. Random rather than the OS's pick: macOS hands out ports in
    sequence, so tests starting together would choose overlapping pairs."""
    for _ in range(100):
        p = random.randrange(20000, 60000)
        if _free(p) and _free(p - 1):
            return p
    raise RuntimeError("no free port pair")


BANNER = re.compile(r"nREPL server started on port (\d+) on host ([\d.]+)")


class Extempore:
    """An extempore process with its nREPL server on a free port. The console
    output is collected in `console` (a list of lines).

    Another process can take the eval ports between choosing them and
    extempore binding them, so a start that dies before it is listening is
    retried with fresh ports."""

    def __init__(
        self,
        *args: str,
        nrepl_port: int | None = 0,
        cwd: Path | None = None,
        wait_for_banner: bool = True,
        timeout: float = 120,
    ):
        self.cwd = cwd or Path(tempfile.mkdtemp(prefix="xtm-nrepl-"))
        self.port = 0
        for attempt in range(3):
            self._launch(args, nrepl_port)
            if not wait_for_banner:
                return
            try:
                self.port = int(self.wait_for(BANNER, timeout).group(1))
                self._wait_until_listening(timeout)
                return
            except RuntimeError:
                if attempt == 2:
                    raise

    def _launch(self, args: tuple[str, ...], nrepl_port: int | None) -> None:
        self.eval_port = free_port_pair()
        cmd = [str(BIN), "--no-audio", "--color", "never", "--share-dir", str(REPO),
               "--legacy-port", str(self.eval_port)]
        if nrepl_port is not None:
            cmd += ["--nrepl-port", str(nrepl_port)]
        cmd += list(args)
        self.proc = subprocess.Popen(cmd, cwd=self.cwd, stdout=subprocess.PIPE,
                                     stderr=subprocess.STDOUT, stdin=subprocess.DEVNULL)
        self.console: list[str] = []
        self._lines = threading.Condition()
        self._exited = threading.Event()
        self._pumping = threading.Thread(target=self._pump, daemon=True)
        self._pumping.start()

    def _wait_until_listening(self, timeout: float) -> None:
        """The primary eval server binds just after the nREPL banner."""
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            if self._exited.is_set():
                raise RuntimeError("extempore exited:\n" + self.console_text())
            try:
                socket.create_connection(("127.0.0.1", self.eval_port), timeout=1).close()
                return
            except OSError:
                time.sleep(0.05)
        raise TimeoutError("extempore never listened:\n" + self.console_text())

    def _pump(self) -> None:
        assert self.proc.stdout
        for raw in self.proc.stdout:
            with self._lines:
                self.console.append(raw.decode(errors="replace"))
                self._lines.notify_all()
        with self._lines:
            self._exited.set()
            self._lines.notify_all()

    def console_text(self) -> str:
        with self._lines:
            return "".join(self.console)

    def wait_for(self, pattern: re.Pattern | str, timeout: float = 30) -> re.Match:
        pattern = re.compile(pattern) if isinstance(pattern, str) else pattern
        deadline = time.monotonic() + timeout
        with self._lines:
            while True:
                m = pattern.search("".join(self.console))
                if m:
                    return m
                if self._exited.is_set():
                    raise RuntimeError(f"extempore exited:\n" + "".join(self.console))
                left = deadline - time.monotonic()
                if left <= 0:
                    raise TimeoutError(f"no {pattern.pattern!r} in console:\n" + "".join(self.console))
                self._lines.wait(min(left, 0.5))

    def client(self, timeout: float = 60) -> client.Client:
        return Client("127.0.0.1", self.port, timeout=timeout)

    def stop(self) -> None:
        if self.proc.poll() is None:
            self.proc.kill()
        self.proc.wait(timeout=30)
        self._pumping.join(timeout=30)
        if self.proc.stdout:
            self.proc.stdout.close()

    def __enter__(self) -> Extempore:
        return self

    def __exit__(self, *_: object) -> None:
        self.stop()


def values(responses: list[dict]) -> list[str]:
    return [r["value"] for r in responses if "value" in r]


def statuses(responses: list[dict]) -> list[str]:
    return [s for r in responses for s in r.get("status", [])]


def out(responses: list[dict]) -> str:
    return "".join(r["out"] for r in responses if "out" in r)

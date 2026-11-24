"""Shared fixtures for the nREPL tests: the client module, and an extempore
process to talk to.

The client is extras/nrepl/extempore-nrepl, loaded as a module. EXTEMPORE_BIN
names the binary (ctest sets it); the default is build/extempore.
"""

from __future__ import annotations

import importlib.machinery
import importlib.util
import os
import re
import socket
import subprocess
import sys
import tempfile
import threading
import time
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
BIN = Path(os.environ.get("EXTEMPORE_BIN", REPO / "build" / "extempore")).resolve()


def _load_client():
    path = REPO / "extras" / "nrepl" / "extempore-nrepl"
    loader = importlib.machinery.SourceFileLoader("extempore_nrepl", str(path))
    spec = importlib.util.spec_from_loader("extempore_nrepl", loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules["extempore_nrepl"] = module
    loader.exec_module(module)
    return module


client = _load_client()
Client = client.Client


def free_port() -> int:
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def free_port_pair() -> int:
    """A port P with P - 1 free too, for extempore's primary and utility
    eval servers."""
    for _ in range(50):
        p = free_port()
        try:
            with socket.socket() as s:
                s.bind(("127.0.0.1", p - 1))
            return p
        except OSError:
            continue
    raise RuntimeError("no free port pair")


BANNER = re.compile(r"nREPL server started on port (\d+) on host ([\d.]+)")


class Extempore:
    """An extempore process with its nREPL server on a free port. The console
    output is collected in `console` (a list of lines)."""

    def __init__(
        self,
        *args: str,
        nrepl_port: int | None = 0,
        cwd: Path | None = None,
        wait_for_banner: bool = True,
        timeout: float = 120,
    ):
        self.cwd = cwd or Path(tempfile.mkdtemp(prefix="xtm-nrepl-"))
        self.eval_port = free_port_pair()
        cmd = [
            str(BIN),
            "--noaudio",
            "--term",
            "nocolor",
            "--sharedir",
            str(REPO),
            "--port",
            str(self.eval_port),
        ]
        if nrepl_port is not None:
            cmd += ["--nrepl-port", str(nrepl_port)]
        cmd += list(args)
        self.proc = subprocess.Popen(
            cmd,
            cwd=self.cwd,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            stdin=subprocess.DEVNULL,
        )
        self.console: list[str] = []
        self._lines = threading.Condition()
        threading.Thread(target=self._pump, daemon=True).start()
        self.port = 0
        if wait_for_banner:
            m = self.wait_for(BANNER, timeout)
            self.port = int(m.group(1))

    def _pump(self) -> None:
        assert self.proc.stdout
        for raw in self.proc.stdout:
            with self._lines:
                self.console.append(raw.decode(errors="replace"))
                self._lines.notify_all()
        with self._lines:
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
                if self.proc.poll() is not None and not self._pump_alive():
                    raise RuntimeError(
                        f"extempore exited ({self.proc.returncode}):\n"
                        + "".join(self.console)
                    )
                left = deadline - time.monotonic()
                if left <= 0:
                    raise TimeoutError(
                        f"no {pattern.pattern!r} in console:\n" + "".join(self.console)
                    )
                self._lines.wait(min(left, 0.5))

    def _pump_alive(self) -> bool:
        return self.proc.stdout is not None and not self.proc.stdout.closed

    def client(self, timeout: float = 60) -> client.Client:
        return Client("127.0.0.1", self.port, timeout=timeout)

    def stop(self) -> None:
        if self.proc.poll() is None:
            self.proc.kill()
        self.proc.wait(timeout=30)

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

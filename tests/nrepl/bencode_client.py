"""A bencode codec and an nREPL connection for the tests.

The tests keep a client of their own, apart from the one in the binary
(src/NreplClient.cpp), because they have to send what no real client would:
malformed bencode, a message a byte at a time, requests whose replies are
never read.
"""

from __future__ import annotations

import itertools
import socket
from typing import Iterator

DEFAULT_PORT = 7888

# --- bencode ---------------------------------------------------------------


def bencode(value: object) -> bytes:
    if isinstance(value, bool):
        raise TypeError("bencode has no booleans")
    if isinstance(value, int):
        return b"i%de" % value
    if isinstance(value, str):
        value = value.encode()
    if isinstance(value, bytes):
        return b"%d:%s" % (len(value), value)
    if isinstance(value, (list, tuple)):
        return b"l" + b"".join(bencode(v) for v in value) + b"e"
    if isinstance(value, dict):
        items = sorted(
            (k.encode() if isinstance(k, str) else k, v) for k, v in value.items()
        )
        return b"d" + b"".join(bencode(k) + bencode(v) for k, v in items) + b"e"
    raise TypeError(f"can't bencode {type(value).__name__}")


class Incomplete(Exception):
    """The buffer ends before the value does."""


def bdecode(data: bytes, pos: int = 0) -> tuple[object, int]:
    """Decode one value at pos; return it and the position after it.

    Byte strings come back as str where they are valid UTF-8."""
    if pos >= len(data):
        raise Incomplete
    c = data[pos : pos + 1]
    if c == b"i":
        end = data.find(b"e", pos)
        if end < 0:
            raise Incomplete
        return int(data[pos + 1 : end]), end + 1
    if c.isdigit():
        colon = data.find(b":", pos)
        if colon < 0:
            raise Incomplete
        n = int(data[pos:colon])
        start, end = colon + 1, colon + 1 + n
        if end > len(data):
            raise Incomplete
        raw = data[start:end]
        try:
            return raw.decode(), end
        except UnicodeDecodeError:
            return raw, end
    if c in (b"l", b"d"):
        items: list[object] = []
        pos += 1
        while True:
            if pos >= len(data):
                raise Incomplete
            if data[pos : pos + 1] == b"e":
                pos += 1
                break
            item, pos = bdecode(data, pos)
            items.append(item)
        if c == b"l":
            return items, pos
        return dict(zip(items[::2], items[1::2])), pos
    raise ValueError(f"bad bencode at byte {pos}: {c!r}")


# --- connection ------------------------------------------------------------


class Client:
    """One connection to the server. Requests get ids; responses to other
    requests are kept until asked for."""

    def __init__(
        self,
        host: str = "127.0.0.1",
        port: int = DEFAULT_PORT,
        timeout: float | None = None,
    ):
        self.sock = socket.create_connection((host, port), timeout=timeout)
        self.buffer = b""
        self.pending: list[dict] = []
        self.ids = itertools.count(1)

    def close(self) -> None:
        self.sock.close()

    def __enter__(self) -> Client:
        return self

    def __exit__(self, *_: object) -> None:
        self.close()

    def send(self, **msg: object) -> str:
        msg.setdefault("id", f"c{next(self.ids)}")
        self.sock.sendall(bencode(msg))
        return str(msg["id"])

    def read(self) -> dict:
        """The next response, whichever request it answers."""
        if self.pending:
            return self.pending.pop(0)
        while True:
            try:
                msg, end = bdecode(self.buffer)
                self.buffer = self.buffer[end:]
                assert isinstance(msg, dict)
                return msg
            except Incomplete:
                chunk = self.sock.recv(65536)
                if not chunk:
                    raise ConnectionError("extempore closed the connection") from None
                self.buffer += chunk

    def responses(self, id: str) -> Iterator[dict]:
        """Responses to request id, up to and including its done."""
        kept: list[dict] = []
        try:
            while True:
                msg = self.read()
                if msg.get("id") != id:
                    kept.append(msg)
                    continue
                yield msg
                if "done" in msg.get("status", []):
                    return
        finally:
            self.pending = kept + self.pending

    def request(self, **msg: object) -> list[dict]:
        return list(self.responses(self.send(**msg)))

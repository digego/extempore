# Vendored oscpp patches

oscpp is vendored verbatim from <https://github.com/kaoskorobase/oscpp> (headers
under `include/oscpp/`, Boost Software License) except for the change recorded
here. Re-apply it when updating.

## `detail/stream.hpp` --- bounds-check before pointer arithmetic in the sub-stream constructor

`Stream(const Stream&, size_t size)` computed `m_end = m_begin + size` _before_
validating `size`. An attacker-controlled bundle element size --- read by
`PacketStream::next()` via `getInt32()`, where a negative int32 widens to a huge
`size_t` --- overflows the pointer. That is undefined behaviour, and aborts a
`-fno-sanitize-recover=undefined` UBSan build (which Extempore ships). Found by
fuzzing the OSC receive path.

The check is reordered to compare `size` against the parent's remaining bytes (a
pointer _difference_, which cannot overflow) before computing `m_end`. Behaviour
is otherwise identical: an oversized sub-stream still throws `UnderrunError`.

Reported upstream: pending.

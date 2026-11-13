### Live capture from a real audio output (pw-record fallback)

When you need to verify the _realtime_ path (not offline), use PipeWire's
`pw-record` with `--eval` (which keeps audio enabled).

### Prerequisites

- PipeWire running (check with `wpctl status`)
- `pw-record` available (from `pipewire` package)
- A PipeWire sink to capture from (the default "Dummy Output" works over SSH)

### Find the sink ID

```bash
wpctl status  # look for the sink ID under "Audio > Sinks"
```

### Capture audio

```bash
# 1. start recording from sink (e.g. sink ID 33)
pw-record --target 33 --rate 44100 --format f32 /tmp/output.wav &
PW_PID=$!
sleep 0.5

# 2. run extempore with audio enabled
timeout 20s ./build/extempore \
  --eval '(sys:load-then-quit "my_dsp_script.xtm" 15)'

# 3. stop recording
kill $PW_PID
wait $PW_PID 2>/dev/null
```

### Analyse the output

```bash
ffprobe /tmp/output.wav 2>&1 | grep -E "Duration|Stream"
ffmpeg -i /tmp/output.wav -af "volumedetect" -f null /dev/null 2>&1 \
  | grep -E "mean_volume|max_volume"
```

### Notes

- `pw-record --target <id>` captures from a specific sink's monitor port
- `--rate 44100 --format f32` matches Extempore's native format (avoids
  resampling); omit these to use PipeWire's default (48000 Hz, s16)
- the first few seconds of the recording will be silence while Extempore loads
  the base library and compiles DSP code
- the ALSA `file` plugin approach (`type file` in `.asoundrc`) does NOT work
  because its `null` slave provides no timing, causing PortAudio's audio clock
  to race ahead and Extempore's load timeouts to expire almost instantly

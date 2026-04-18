---
id: task-5
title: sort out EXT_SHARE_DIR and other env vars
status: To Do
assignee: []
created_date: '2025-12-16 10:38'
updated_date: '2025-12-17 05:52'
labels: []
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Consolidate environment variables to use consistent `EXTEMPORE_*` naming, add runtime path override via `EXTEMPORE_PATH`, and implement default CLI args via `EXTEMPORE_ARGS`.
<!-- SECTION:DESCRIPTION:END -->

It'd be simpler to just move to:

- EXTEMPORE_PATH (same as EXT_SHARE_DIR, with the latter printing a deprecation
  warning but still working)
- EXTEMPORE_ARGS (a string of default args... as if they'd been passed to the
  command line)

There are a few other EXT\_\* vars (most for the build process, but some for
runtime as well I think...) and we should do a thorough audit to see if they're
still needed or can be removed.

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 EXTEMPORE_PATH env var sets share directory at runtime
- [ ] #2 Deprecated EXT_SHARE_DIR env var still works but prints warning to stderr
- [ ] #3 --sharedir CLI arg overrides both env vars
- [ ] #4 EXTEMPORE_ARGS env var provides default arguments
- [ ] #5 CLI arguments override values from EXTEMPORE_ARGS
- [ ] #6 EXTEMPORE_MIDI_{IN,OUT}_DEVICE env vars work with deprecation fallback to old names
- [ ] #7 Dead get-llvm-path function and EXT_LLVM_DIR reference removed from runtime/llvmti.xtm
- [ ] #8 --help output documents all EXTEMPORE_* env vars
- [ ] #9 All existing tests pass
<!-- AC:END -->

## Implementation Plan

<!-- SECTION:PLAN:BEGIN -->
## Background

### Current state of SHARE_DIR

The share directory (containing `runtime/`, `libs/`, `examples/`, etc.) is currently handled as follows:

1. **Compile-time default**: CMake defines `EXT_SHARE_DIR` macro (usually `CMAKE_SOURCE_DIR` for dev builds)
2. **Runtime initialisation**: `src/UNIV.cpp:562` initialises `SHARE_DIR` from this macro
3. **CLI override**: `--sharedir` flag can override at runtime (`src/Extempore.cpp:208`)

The compile-time default works for development but is fragile for distributed binaries --- if the binary is moved, it won't find its resources unless `--sharedir` is explicitly passed.

### Audit results

**Runtime environment variables (currently used):**
| Variable | Location | Status |
|----------|----------|--------|
| `EXT_LLVM_DIR` | `runtime/llvmti.xtm` | Dead code --- `get-llvm-path` is defined but never called. Delete. |
| `EXT_MIDI_IN_DEVICE_NAME` | `examples/sharedsystem/midisetup.xtm` | Rename to `EXTEMPORE_MIDI_IN_DEVICE` |
| `EXT_MIDI_OUT_DEVICE_NAME` | `examples/sharedsystem/midisetup.xtm` | Rename to `EXTEMPORE_MIDI_OUT_DEVICE` |

**Build-time CMake variables (no changes needed):**
| Variable | Purpose |
|----------|---------|
| `EXT_SHARE_DIR` | CMake define for compile-time default path |
| `EXT_DYLIB` | CMake option to build as dynamic library |
| `EXTEMPORE_FORCE_GL_GETPROCADDRESS` | Build-time env var (already uses new naming) |

**Internal C++ identifiers (not environment variables, no changes needed):**
| Identifier | Purpose |
|------------|---------|
| `EXT_TERM` | Terminal colour mode (0=ansi, 1=cmd, 2=basic, 3=nocolor) |
| `EXT_LOADBASE` | Whether to load base library at startup |
| `EXT_INITEXPR_BUFLEN` | Buffer size constant |
| `EXT_Thread/Mutex/Condition/Monitor` | Header guard macros for threading classes |

---

## Implementation plan

### Phase 1: add EXTEMPORE_PATH runtime env var

**Goal:** Allow setting the share directory via environment variable, with backwards compatibility.

**Files to modify:**
- `src/Extempore.cpp`

**Changes:**

In `extempore_init()`, before CLI argument parsing begins, add env var checking:

```cpp
// Check for EXTEMPORE_PATH env var (new)
const char* env_path = std::getenv("EXTEMPORE_PATH");
if (env_path && strlen(env_path) > 0) {
    extemp::UNIV::SHARE_DIR = std::string(env_path);
} else {
    // Check for deprecated EXT_SHARE_DIR env var
    const char* old_env_path = std::getenv("EXT_SHARE_DIR");
    if (old_env_path && strlen(old_env_path) > 0) {
        ascii_warning();
        std::cout << "Warning: ";
        ascii_default();
        std::cout << "EXT_SHARE_DIR is deprecated, use EXTEMPORE_PATH instead" << std::endl;
        extemp::UNIV::SHARE_DIR = std::string(old_env_path);
    }
    // Otherwise keep compile-time default (already set in UNIV.cpp)
}
```

**Priority order (highest wins):**
1. `--sharedir` CLI argument
2. `EXTEMPORE_PATH` env var
3. `EXT_SHARE_DIR` env var (deprecated, prints warning)
4. Compile-time `EXT_SHARE_DIR` macro default

### Phase 2: add EXTEMPORE_ARGS env var

**Goal:** Allow setting default CLI arguments via environment variable.

**Files to modify:**
- `src/Extempore.cpp`

**Changes:**

In `extempore_init()`, before `CSimpleOptA args(argc, argv, g_rgOptions)`:

1. Read `EXTEMPORE_ARGS` env var
2. If set, tokenise it (space-separated, respecting quoted strings)
3. Build a new argv array: `[argv[0], ...env_tokens, ...argv[1:]]`
4. Pass the combined array to SimpleOpt

```cpp
std::vector<char*> combined_argv;
combined_argv.push_back(argv[0]); // program name

// Parse EXTEMPORE_ARGS if set
const char* env_args = std::getenv("EXTEMPORE_ARGS");
std::vector<std::string> env_tokens; // keep strings alive
if (env_args && strlen(env_args) > 0) {
    env_tokens = tokenize_args(env_args); // helper function needed
    for (auto& tok : env_tokens) {
        combined_argv.push_back(const_cast<char*>(tok.c_str()));
    }
}

// Add actual CLI args (these override env args due to SimpleOpt's last-wins behaviour)
for (int i = 1; i < argc; i++) {
    combined_argv.push_back(argv[i]);
}

CSimpleOptA args(combined_argv.size(), combined_argv.data(), g_rgOptions);
```

**Helper function to add:**

```cpp
// Tokenise a string, respecting double-quoted substrings
std::vector<std::string> tokenize_args(const char* str) {
    std::vector<std::string> tokens;
    std::string current;
    bool in_quotes = false;
    
    for (const char* p = str; *p; ++p) {
        if (*p == '"') {
            in_quotes = !in_quotes;
        } else if (*p == ' ' && !in_quotes) {
            if (!current.empty()) {
                tokens.push_back(current);
                current.clear();
            }
        } else {
            current += *p;
        }
    }
    if (!current.empty()) {
        tokens.push_back(current);
    }
    return tokens;
}
```

**Example usage:**
```bash
export EXTEMPORE_ARGS="--noaudio --port 7100"
./extempore  # runs with --noaudio --port 7100

./extempore --port 7099  # CLI overrides: uses port 7099 but still --noaudio
```

### Phase 3: rename MIDI env vars in xtlang

**Goal:** Consistent `EXTEMPORE_*` naming with deprecation support.

**Files to modify:**
- `examples/sharedsystem/midisetup.xtm`

**Changes:**

Replace direct `sys:get-env` calls with a helper that checks both names:

```scheme
(define get-env-with-fallback
  (lambda (new-name old-name)
    (let ((new-val (sys:get-env new-name))
          (old-val (sys:get-env old-name)))
      (cond
        (new-val new-val)
        (old-val
          (print-with-colors 'yellow 'default #t (print "Warning"))
          (print " " old-name " is deprecated, use " new-name " instead\n")
          old-val)
        (else #f)))))

;; Then use:
(get-env-with-fallback "EXTEMPORE_MIDI_OUT_DEVICE" "EXT_MIDI_OUT_DEVICE_NAME")
(get-env-with-fallback "EXTEMPORE_MIDI_IN_DEVICE" "EXT_MIDI_IN_DEVICE_NAME")
```

### Phase 4: remove dead code

**Goal:** Clean up unused `get-llvm-path` function.

**Files to modify:**
- `runtime/llvmti.xtm`

**Changes:**

Delete the `get-llvm-path` function (lines ~2857-2869). It references `EXT_LLVM_DIR` but is never called anywhere in the codebase. LLVM is linked at build time; there's no runtime need to locate LLVM files.

### Phase 5: update documentation

**Files to modify:**
- `src/Extempore.cpp` (the `--help` output)

**Changes to `--help`:**

Add a new section after the options list:

```cpp
std::cout << std::endl;
std::cout << "Environment variables:" << std::endl;
std::cout << "  EXTEMPORE_PATH: path to Extempore share directory (runtime/, libs/, etc.)" << std::endl;
std::cout << "  EXTEMPORE_ARGS: default command-line arguments" << std::endl;
std::cout << "  EXTEMPORE_MIDI_IN_DEVICE: default MIDI input device name" << std::endl;
std::cout << "  EXTEMPORE_MIDI_OUT_DEVICE: default MIDI output device name" << std::endl;
```

---

## Final environment variables

| Variable | Purpose | Fallback chain |
|----------|---------|----------------|
| `EXTEMPORE_PATH` | Share directory path | → `EXT_SHARE_DIR` env (deprecated) → compile-time default |
| `EXTEMPORE_ARGS` | Default CLI arguments | (none) |
| `EXTEMPORE_MIDI_IN_DEVICE` | MIDI input device name | → `EXT_MIDI_IN_DEVICE_NAME` (deprecated) → (none) |
| `EXTEMPORE_MIDI_OUT_DEVICE` | MIDI output device name | → `EXT_MIDI_OUT_DEVICE_NAME` (deprecated) → (none) |

## Removed

| Variable | Reason |
|----------|--------|
| `EXT_LLVM_DIR` | Dead code --- `get-llvm-path` never called; LLVM linked at build time |
<!-- SECTION:PLAN:END -->

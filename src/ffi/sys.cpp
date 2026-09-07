/*
 * Copyright (c) 2011, Andrew Sorensen
 *
 * All rights reserved.
 *
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * Neither the name of the authors nor other contributors may be used to endorse
 * or promote products derived from this software without specific prior written
 * permission.
 *
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include "SchemeFFIRegistry.h"

#include "SchemeProcess.h"
#include "UNIV.h"

#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <ios>
#include <iostream>
#include <string>
#include <system_error>

// UNIV.h supplies <Windows.h> (behind WIN32_LEAN_AND_MEAN) for the DLL
// helpers; the POSIX build needs dlfcn itself.
#ifndef _WIN32
#include <dlfcn.h>
#endif

namespace extemp {

namespace SchemeFFI {

static pointer pointerSize(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, 8 * sizeof(uintptr_t));
}

static pointer mcjitEnabled(scheme* Scheme, pointer Args)
{
    return Scheme->T;
}

static pointer platform(scheme* Scheme, pointer Args)
{
#ifdef __APPLE__
    return mk_string(Scheme, "OSX");
#elif __linux__
    return mk_string(Scheme, "Linux");
#elif _WIN32
    return mk_string(Scheme, "Windows");
#else
    return mk_string(Scheme, "unknown");
#endif
}

static pointer getShareDir(scheme* Scheme, pointer Args)
{
    return mk_string(Scheme, UNIV::SHARE_DIR.c_str());
}

static pointer cmdarg(scheme* Scheme, pointer Args)
{
    auto iter(UNIV::CMDPARAMS.find(argString(Scheme, Args, 1)));
    if (iter == UNIV::CMDPARAMS.end()) {
        return Scheme->F;
    }
    return mk_string(Scheme, iter->second.c_str());
}

static pointer openDynamicLib(scheme* Scheme, pointer Args)
{
    const char* libname = argString(Scheme, Args, 1);
#ifdef _WIN32
    // set up the DLL load path
    SetDllDirectory(""); // Plug "binary planting" security hole.
    if (!SetDllDirectory((extemp::UNIV::SHARE_DIR + "/libs/aot-cache").c_str())) {
        std::cout << "Warning: couldn't add libs/aot-cache/ to DLL search path" << std::endl;
    }
    void* lib_handle = LoadLibraryA(libname);
#else
    void* lib_handle = dlopen(libname, RTLD_LAZY|RTLD_GLOBAL);  // TODO: RTLD_NOW
#endif
    if (!lib_handle) {
        // if an optional second argument is non-nil, print the error
        if (pair_cdr(Args) != Scheme->NIL && pair_cadr(Args) != Scheme->F)
        {
#ifdef _WIN32
            std::cout << "LoadLibraryA() error:" << GetLastError() << std::endl;
#else
            printf("dlopen() error: %s\n", dlerror());
#endif
        }
        return Scheme->F;
    }
    return mk_cptr(Scheme, lib_handle);
}

static pointer closeDynamicLib(scheme* Scheme, pointer Args)
{
#ifdef _WIN32
    FreeLibrary((HMODULE)argCptr(Scheme, Args, 1));
#else
    dlclose(argCptr(Scheme, Args, 1));
#endif
    return Scheme->T;
}

static pointer symbol_pointer(scheme* Scheme, pointer Args)
{
    void* library = argCptr(Scheme, Args, 1);
    char* symname = argString(Scheme, Args, 2);
#ifdef _WIN32
    void* ptr = (void*) GetProcAddress(HMODULE(library), symname);
#else
    void* ptr = dlsym(library, symname);
#endif
    if (!ptr) {
        return Scheme->F;
    }
    return mk_cptr(Scheme, ptr);
}

static pointer makeCptr(scheme* Scheme, pointer Args)
{
     auto num_bytes(argInt(Scheme, Args, 1));
     if (num_bytes <= 0) {
         return Scheme->F;
     }
     void* ptr = calloc(1, size_t(num_bytes));
     if (!ptr) {
         return Scheme->F;
     }
     return mk_cptr(Scheme, ptr);
}

static pointer slurpFile(scheme* Scheme, pointer Args)
{
    std::string filename(argString(Scheme, Args, 1));

    // check raw path first, then prepend SHARE_DIR
    std::ifstream in(filename, std::ios::binary);
    if (!in) {
        in.clear();
        in.open(UNIV::SHARE_DIR + "/" + filename, std::ios::binary);
    }
    if (!in) {
        return Scheme->F;
    }
    in.seekg(0, std::ios::end);
    std::streamoff length = in.tellg();
    if (length < 0) {
        return Scheme->F;
    }
    std::string contents(static_cast<size_t>(length), '\0');
    in.seekg(0, std::ios::beg);
    in.read(&contents[0], length);
    if (in.bad()) {
        return Scheme->F;
    }
    contents.resize(static_cast<size_t>(in.gcount()));
    return mk_counted_string(Scheme, contents.data(), int(contents.size()));
}

static pointer dumpStringToFile(scheme* Scheme, pointer Args)
{
    std::string filename(argString(Scheme, Args, 1));
    pointer str = argAt(Scheme, Args, 2);
    if (!is_string(str)) {
        ffiWrongType(str, 2, "a string");
    }

    std::ofstream out(filename, std::ios::binary);
    if (!out) {
        return Scheme->F;
    }
    out.write(s7_string(str), s7_string_length(str));
    out.close();
    return out ? Scheme->T : Scheme->F;
}

static pointer dirlist(scheme* Scheme, pointer Args)
{
    // Bare filenames, and no "." or ".." (directory_iterator skips those):
    // callers such as libs/external/instruments_ext-scm.xtm match the entries
    // against a filename pattern, not a path.
    std::error_code ec;
    std::filesystem::path dir(argString(Scheme, Args, 1));
    pointer list = Scheme->NIL;
    for (std::filesystem::directory_iterator it(dir, ec), end; !ec && it != end; it.increment(ec)) {
        EnvInjector injector(Scheme, list);
        pointer s = mk_string(Scheme, it->path().filename().string().c_str());
        EnvInjector injector2(Scheme, s);
        list = cons(Scheme, s, list);
    }
    if (ec) {
        return Scheme->NIL;
    }
    return reverse(Scheme, list);
}

static pointer pathExpansion(scheme* Scheme, pointer Args)
{
    std::string path(argString(Scheme, Args, 1));
    if (!path.empty() && path[0] == '~') {
        const char* home = std::getenv("HOME");
#ifdef _WIN32
        if (!home) {
            home = std::getenv("USERPROFILE");
        }
#endif
        // no home to expand against: leave the path as the caller wrote it
        if (home) {
            path = std::string(home) + path.substr(1);
        }
    }
    std::error_code ec;
    std::filesystem::path expanded(std::filesystem::weakly_canonical(path, ec));
    if (ec) {
        return mk_string(Scheme, path.c_str());
    }
    return mk_string(Scheme, expanded.string().c_str());
}

static pointer command(scheme* Scheme, pointer Args)
{
    // NOTE: doesn't work for Windows yet
    return mk_integer(Scheme, system(argString(Scheme, Args, 1)));
}

static pointer commandOutput(scheme* Scheme, pointer Args)
{
    const char* cmd = argString(Scheme, Args, 1);
#ifdef _WIN32
    FILE* stream = _popen(cmd, "r");
#else
    FILE* stream = popen(cmd, "r");
#endif
    if (!stream) {
        return Scheme->F;
    }
    std::string out;
    char buf[4096];
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), stream)) > 0) {
        out.append(buf, n);
    }
#ifdef _WIN32
    _pclose(stream);
#else
    pclose(stream);
#endif
    // #f means the command wrote nothing at all --- a command whose output is
    // just a newline still yields "" (callers such as the LD_LIBRARY_PATH
    // lookup in runtime/xtc-bind.xtm split the result, and can't split #f)
    if (out.empty()) {
        return Scheme->F;
    }
    // get rid of the trailing newline
    while (!out.empty() && (out.back() == '\n' || out.back() == '\r')) {
        out.pop_back();
    }
    return mk_counted_string(Scheme, out.data(), int(out.size()));
}

static pointer getEnv(scheme* Scheme, pointer Args)
{
  const char* var = argString(Scheme, Args, 1);
  const char* val = std::getenv(var);

  if (val != NULL){
	return mk_string(Scheme, val);
  }else{
    return Scheme->F;
  }
}

static pointer setEnv(scheme* Scheme, pointer Args)
{
    char* var = argString(Scheme, Args, 1);
    char* val = argString(Scheme, Args, 2);

    int res;
#ifdef _WIN32
    res = _putenv_s(var, val);
#else
    res = setenv(var, val, 1); // overwrite = TRUE
#endif
    return mk_integer(Scheme, res);
}

static pointer setDefaultTimeout(scheme* Scheme, pointer Args)
{
    Scheme->m_process->setMaxDuration(argInt(Scheme, Args, 1));
    return Scheme->T;
}

static pointer getDefaultTimeout(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, Scheme->m_process->getMaxDuration());
}

std::span<const FFIEntry> sysDefs()
{
    static const FFIEntry defs[] = {
        FFI_DEF("sys:pointer-size", pointerSize, 0, 0, false),
        FFI_DEF("sys:mcjit-enabled", mcjitEnabled, 0, 0, false),
        FFI_DEF("sys:platform", platform, 0, 0, false),
        FFI_DEF("sys:share-dir", getShareDir, 0, 0, false),
        FFI_DEF("sys:cmdarg", cmdarg, 1, 0, false),
        FFI_DEF("sys:open-dylib", openDynamicLib, 1, 1, false),
        FFI_DEF("sys:close-dylib", closeDynamicLib, 1, 0, false),
        FFI_DEF("sys:symbol-cptr", symbol_pointer, 2, 0, false),
        FFI_DEF("sys:make-cptr", makeCptr, 1, 0, false),
        FFI_DEF("sys:slurp-file", slurpFile, 1, 0, false),
        FFI_DEF("sys:dump-string-to-file", dumpStringToFile, 2, 0, false),
        FFI_DEF("sys:directory-list", dirlist, 1, 0, false),
        FFI_DEF("sys:expand-path", pathExpansion, 1, 0, false),
        FFI_DEF("sys:command", command, 1, 0, false),
        FFI_DEF("sys:command-output", commandOutput, 1, 0, false),
        FFI_DEF("sys:get-env", getEnv, 1, 0, false),
        FFI_DEF("sys:set-env", setEnv, 2, 0, false),
        FFI_DEF("sys:set-default-timeout", setDefaultTimeout, 1, 0, false),
        FFI_DEF("sys:get-default-timeout", getDefaultTimeout, 0, 0, false),
    };
    return defs;
}

}  // namespace SchemeFFI

}  // namespace extemp

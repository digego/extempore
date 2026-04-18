#ifndef LINENOISE_REPL_H
#define LINENOISE_REPL_H

#ifndef _WIN32

#include <string>

struct linenoise_repl_args {
    std::string host;
    int port;
};

void* linenoise_repl(void* dat);

#endif
#endif

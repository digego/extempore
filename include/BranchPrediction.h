#if !defined(_BRANCH_PREDICTION_H)
#define _BRANCH_PREDICTION_H

#ifdef _WIN32
#define likely(X) (X)
#define unlikely(X) (X)
#else //!_WIN32
#define likely(X) __builtin_expect(!!(X), 1)
#define unlikely(X) __builtin_expect(!!(X), 0)
#endif

#endif
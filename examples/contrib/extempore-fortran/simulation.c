/* C struct declaration */
struct pass {int lenc, lenf; float* f, *c};

/* C function prototype */
void simulation(long alpha, double *beta, long *gamma, double delta[],  struct pass *arrays);

/* C calling sequence */
simulation(alpha, &beta, &gamma, delta, &arrays);

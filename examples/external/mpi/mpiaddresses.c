#include <mpi.h>
#include <stdio.h>

struct ompi_predefined_datatype_t *ompi_mpi_datatype_null_addr = &ompi_mpi_datatype_null;
/* struct ompi_predefined_datatype_t *ompi_mpi_unavailable_addr = &ompi_mpi_unavailable; */

struct ompi_predefined_datatype_t *ompi_mpi_char_addr = &ompi_mpi_char;
struct ompi_predefined_datatype_t *ompi_mpi_signed_char_addr = &ompi_mpi_signed_char;
struct ompi_predefined_datatype_t *ompi_mpi_unsigned_char_addr = &ompi_mpi_unsigned_char;
struct ompi_predefined_datatype_t *ompi_mpi_byte_addr = &ompi_mpi_byte;
struct ompi_predefined_datatype_t *ompi_mpi_short_addr = &ompi_mpi_short;
struct ompi_predefined_datatype_t *ompi_mpi_unsigned_short_addr = &ompi_mpi_unsigned_short;
struct ompi_predefined_datatype_t *ompi_mpi_int_addr = &ompi_mpi_int;
struct ompi_predefined_datatype_t *ompi_mpi_unsigned_addr = &ompi_mpi_unsigned;
struct ompi_predefined_datatype_t *ompi_mpi_long_addr = &ompi_mpi_long;
struct ompi_predefined_datatype_t *ompi_mpi_unsigned_long_addr = &ompi_mpi_unsigned_long;
struct ompi_predefined_datatype_t *ompi_mpi_long_long_int_addr = &ompi_mpi_long_long_int;
struct ompi_predefined_datatype_t *ompi_mpi_unsigned_long_long_addr = &ompi_mpi_unsigned_long_long;
struct ompi_predefined_datatype_t *ompi_mpi_float_addr = &ompi_mpi_float;
struct ompi_predefined_datatype_t *ompi_mpi_double_addr = &ompi_mpi_double;
struct ompi_predefined_datatype_t *ompi_mpi_long_double_addr = &ompi_mpi_long_double;
struct ompi_predefined_datatype_t *ompi_mpi_wchar_addr = &ompi_mpi_wchar;
struct ompi_predefined_datatype_t *ompi_mpi_packed_addr = &ompi_mpi_packed;
/*
 * C++ / C99 datatypes
 */
struct ompi_predefined_datatype_t *ompi_mpi_c_bool_addr = &ompi_mpi_c_bool;
struct ompi_predefined_datatype_t *ompi_mpi_cxx_bool_addr = &ompi_mpi_cxx_bool;
/*
 * Complex datatypes for C (base types), C++, and fortran
 */
struct ompi_predefined_datatype_t *ompi_mpi_c_float_complex_addr = &ompi_mpi_c_float_complex;
struct ompi_predefined_datatype_t *ompi_mpi_c_complex_addr = &ompi_mpi_c_complex;
struct ompi_predefined_datatype_t *ompi_mpi_c_double_complex_addr = &ompi_mpi_c_double_complex;
struct ompi_predefined_datatype_t *ompi_mpi_c_long_double_complex_addr = &ompi_mpi_c_long_double_complex;

/* extern void print_struct_data(void) */
/* { */
/*   int i; */
/*   unsigned char *data = (unsigned char *)MPI_INT; */
/*   for (i;) { */
/*     printf("%u ", data[i]); */
/*   } */
/*   printf("\n"); */
/* } */

extern void* get_MPI_COMM_WORLD_pointer(void)
{
  return (void *)MPI_COMM_WORLD;
}

extern void* get_MPI_INT_pointer(void)
{
  return (void *)MPI_INT;
}

/* Local Variables: */
/* compile-command: "clang -dynamiclib -o libmpiaddresses.dylib mpiaddresses.c -lmpi" */
/* End: */

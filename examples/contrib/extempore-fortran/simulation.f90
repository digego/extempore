SUBROUTINE SIMULATION(ALPHA, BETA, GAMMA, DELTA, ARRAYS) BIND(C)
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER (C_LONG), VALUE                 :: ALPHA
    REAL (C_DOUBLE), INTENT(INOUT)          :: BETA
    INTEGER (C_LONG), INTENT(OUT)           :: GAMMA
    REAL (C_DOUBLE),DIMENSION(*),INTENT(IN) :: DELTA
    TYPE, BIND(C) :: PASS
        INTEGER (C_INT) :: LENC, LENF
        TYPE (C_PTR)    :: C, F
    END TYPE PASS
    TYPE (PASS), INTENT(INOUT) :: ARRAYS
    REAL (C_FLOAT), ALLOCATABLE, TARGET, SAVE :: ETA(:)
    REAL (C_FLOAT), POINTER :: C_ARRAY(:)
    write(*,*) "Hello World!"
    ! Associate C_ARRAY with an array allocated in C
    CALL C_F_POINTER (ARRAYS%C, C_ARRAY, (/ARRAYS%LENC/) )

    ! Allocate an array and make it available in C
    ARRAYS%LENF = 100
    ALLOCATE (ETA(ARRAYS%LENF))
    ARRAYS%F = C_LOC(ETA)

END SUBROUTINE SIMULATION

! call with gfortran -shared -fPIC -o simulation.dylib simulation.f90

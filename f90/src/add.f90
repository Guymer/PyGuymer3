PURE SUBROUTINE add(a, b, c)
    ! Import standard modules ...
    USE ISO_C_BINDING

    IMPLICIT NONE

    ! Declare inputs/outputs ...
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: a
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: b
    REAL(kind = C_DOUBLE), INTENT(out)                                          :: c

    ! Add them together ...
    c = a + b
END SUBROUTINE add

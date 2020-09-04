PURE SUBROUTINE buffer_point_crudely(lon1, lat1, dist, nang, ring)
    ! Import standard modules ...
    USE ISO_C_BINDING

    ! Import my modules ...
    USE mod_safe, ONLY: sub_calc_loc_from_loc_and_bearing_and_dist

    IMPLICIT NONE

    ! Declare inputs/outputs ...
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: nang
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: dist
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: lat1
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: lon1
    REAL(kind = C_DOUBLE), DIMENSION(nang, 2), INTENT(out)                      :: ring

    ! Declare internal variables ...
    INTEGER(kind = C_LONG_LONG)                                                 :: iang
    REAL(kind = C_DOUBLE)                                                       :: ang1
    REAL(kind = C_DOUBLE)                                                       :: ang2
    REAL(kind = C_DOUBLE)                                                       :: lat2
    REAL(kind = C_DOUBLE)                                                       :: lon2

    ! Loop over angles ...
    ! NOTE: The first and last angles will *always* be exactly North.
    ! NOTE: The most two subsequent points can be apart is ~45 degrees (with
    !       nang >= 9).
    DO iang = 1_C_LONG_LONG, nang
        ! Calculate initial angle, then the ring coordinates and add them to the
        ! list ...
        ang1 = 360.0e0_C_DOUBLE * REAL(iang - 1_C_LONG_LONG, kind = C_DOUBLE) / REAL(nang - 1_C_LONG_LONG, kind = C_DOUBLE)
        CALL sub_calc_loc_from_loc_and_bearing_and_dist(lon1, lat1, ang1, dist, lon2, lat2, ang2)
        ring(iang, 1) = lon2
        ring(iang, 2) = lat2
    END DO
END SUBROUTINE buffer_point_crudely

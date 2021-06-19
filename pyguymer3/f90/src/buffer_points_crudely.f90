PURE SUBROUTINE buffer_points_crudely(points1, dist, nang, npoint, points2)
    ! Import standard modules ...
    USE ISO_C_BINDING

    ! Import my modules ...
    USE mod_safe, ONLY: sub_calc_loc_from_loc_and_bearing_and_dist

    IMPLICIT NONE

    ! Declare inputs/outputs ...
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: nang
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: npoint
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: dist
    REAL(kind = C_DOUBLE), DIMENSION(npoint, 2), INTENT(in)                     :: points1
    REAL(kind = C_DOUBLE), DIMENSION(npoint, nang, 2), INTENT(out)              :: points2

    ! Declare internal variables ...
    INTEGER(kind = C_LONG_LONG)                                                 :: iang
    INTEGER(kind = C_LONG_LONG)                                                 :: ipoint
    REAL(kind = C_DOUBLE)                                                       :: ang1
    REAL(kind = C_DOUBLE)                                                       :: ang2

    ! Loop over angles ...
    ! NOTE: The first and last angles will *always* be exactly North.
    ! NOTE: The most two subsequent points can be apart is ~45 degrees (with
    !       nang >= 9).
    DO iang = 1_C_LONG_LONG, nang
        ! Calculate initial angle ...
        ang1 = 360.0e0_C_DOUBLE * REAL(iang - 1_C_LONG_LONG, kind = C_DOUBLE) / REAL(nang - 1_C_LONG_LONG, kind = C_DOUBLE)

        ! Loop over points ...
        DO ipoint = 1_C_LONG_LONG, npoint
            ! Calculate the ring coordinates and add them to the array ...
            CALL sub_calc_loc_from_loc_and_bearing_and_dist(points1(ipoint, 1), points1(ipoint, 2), ang1, dist, points2(ipoint, iang, 1), points2(ipoint, iang, 2), ang2)
        END DO
    END DO
END SUBROUTINE buffer_points_crudely

SUBROUTINE buffer_point_crudely(lon1, lat1, dist, nang, ring, tol)
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
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: tol
    REAL(kind = C_DOUBLE), DIMENSION(nang, 2), INTENT(out)                      :: ring

    ! Declare internal variables ...
    INTEGER(kind = C_LONG_LONG)                                                 :: firang
    INTEGER(kind = C_LONG_LONG)                                                 :: iang
    INTEGER(kind = C_LONG_LONG)                                                 :: iang2
    INTEGER(kind = C_LONG_LONG)                                                 :: iang3
    INTEGER(kind = C_LONG_LONG)                                                 :: lasang
    INTEGER(kind = C_LONG_LONG)                                                 :: midang
    REAL(kind = C_DOUBLE)                                                       :: ang1
    REAL(kind = C_DOUBLE)                                                       :: ang2
    REAL(kind = C_DOUBLE)                                                       :: dlon1
    REAL(kind = C_DOUBLE)                                                       :: dlon3
    REAL(kind = C_DOUBLE)                                                       :: endLat1
    REAL(kind = C_DOUBLE)                                                       :: endLat2
    REAL(kind = C_DOUBLE)                                                       :: endLon1
    REAL(kind = C_DOUBLE)                                                       :: endLon2
    REAL(kind = C_DOUBLE), ALLOCATABLE, DIMENSION(:, :)                         :: points3

    ! Create short-hands ...
    firang = (nang - 1_C_LONG_LONG) / 4_C_LONG_LONG + 1_C_LONG_LONG
    midang = (nang - 1_C_LONG_LONG) / 2_C_LONG_LONG + 1_C_LONG_LONG
    lasang = 3_C_LONG_LONG * (nang - 1_C_LONG_LONG) / 4_C_LONG_LONG + 1_C_LONG_LONG

    !$omp parallel                                                              &
    !$omp default(none)                                                         &
    !$omp private(ang1)                                                         &
    !$omp private(ang2)                                                         &
    !$omp private(iang)                                                         &
    !$omp shared(dist)                                                          &
    !$omp shared(lat1)                                                          &
    !$omp shared(lon1)                                                          &
    !$omp shared(nang)                                                          &
    !$omp shared(ring)
        !$omp do                                                                &
        !$omp schedule(dynamic)
            ! Loop over angles ...
            ! NOTE: The first and last angles will *always* be exactly North
            !       (therefore, use that as a check later on).
            ! NOTE: The middle angle will *always* be exactly South (therefore,
            !       use that as a check later on).
            ! NOTE: The most two subsequent points can be apart is ~45° (with
            !       nang ≥ 9).
            DO iang = 1_C_LONG_LONG, nang
                ! Calculate initial angle, then the ring coordinates and add
                ! them to the list ...
                ang1 = 360.0e0_C_DOUBLE * REAL(iang - 1_C_LONG_LONG, kind = C_DOUBLE) / REAL(nang - 1_C_LONG_LONG, kind = C_DOUBLE)
                CALL sub_calc_loc_from_loc_and_bearing_and_dist(lon1, lat1, ang1, dist, ring(iang, 1), ring(iang, 2), ang2)
            END DO
        !$omp end do
    !$omp end parallel

    ! Create short-hand ...
    dlon1 = ABS(lon1 - ring(1, 1))                                              ! [°]

    ! Check if it goes over the North Pole ...
    IF(dlon1 >= tol)THEN
        ! Allocate array ...
        ALLOCATE(points3(nang, 2))                                              ! [°]

        ! Fix the first point ...
        points3(1, 1) = ring(1, 1)                                              ! [°]
        points3(1, 2) = 90.0e0_C_DOUBLE                                         ! [°]

        ! Fix the last point ...
        points3(nang, 1) = ring(nang, 1)                                        ! [°]
        points3(nang, 2) = 90.0e0_C_DOUBLE                                      ! [°]

        ! Fill in the first-half and the second-half of the points (missing out
        ! 3 o'clock and 9 o'clock to make space for the two new points) ...
        iang3 = 2_C_LONG_LONG
        DO iang2 = 1_C_LONG_LONG, nang
            IF(iang2 == firang .OR. iang2 == lasang)THEN
                CYCLE
            END IF
            points3(iang3, 1) = ring(iang2, 1)                                  ! [°]
            points3(iang3, 2) = ring(iang2, 2)                                  ! [°]
            iang3 = iang3 + 1_C_LONG_LONG
        END DO

        ! Overwrite the points ...
        DO iang = 1_C_LONG_LONG, nang
            ring(iang, 1) = points3(iang, 1)                                    ! [°]
            ring(iang, 2) = points3(iang, 2)                                    ! [°]
        END DO

        ! Deallocate array ...
        DEALLOCATE(points3)

        ! Return ...
        RETURN
    END IF

    ! Create short-hand ...
    dlon3 = ABS(lon1 - ring(midang, 1))                                         ! [°]

    ! Check if it goes over the South Pole ...
    if(dlon3 >= tol)THEN
        ! Allocate array ...
        ALLOCATE(points3(nang, 2))                                              ! [°]

        ! Fill in the first-half of the points (missing out 3 o'clock to make
        ! space for the three new points) ...
        iang3 = 1_C_LONG_LONG
        DO iang2 = 1_C_LONG_LONG, midang - 1_C_LONG_LONG
            IF(iang2 == firang)THEN
                CYCLE
            END IF
            points3(iang3, 1) = ring(iang2, 1)                                  ! [°]
            points3(iang3, 2) = ring(iang2, 2)                                  ! [°]
            iang3 = iang3 + 1_C_LONG_LONG
        END DO

        ! Replace the single point with a pair of points 0.05° either side of it
        ! (this is required because the point is not duplicated, unlike at the
        ! North Pole) ...
        CALL sub_calc_loc_from_loc_and_bearing_and_dist(lon1, lat1, 179.95e0_C_DOUBLE, dist, endLon1, endLat1, ang2)
        CALL sub_calc_loc_from_loc_and_bearing_and_dist(lon1, lat1, 180.05e0_C_DOUBLE, dist, endLon2, endLat2, ang2)

        ! Add the point before it crosses the South Pole ...
        points3(iang3, 1) = endLon1                                             ! [°]
        points3(iang3, 2) = endLat1                                             ! [°]
        iang3 = iang3 + 1_C_LONG_LONG

        ! Fix the point before it crosses the South Pole ...
        points3(iang3, 1) = endLon1                                             ! [°]
        points3(iang3, 2) = -90.0e0_C_DOUBLE                                    ! [°]
        iang3 = iang3 + 1_C_LONG_LONG

        ! Fix the point after it crosses the South Pole ...
        points3(iang3, 1) = endLon2                                             ! [°]
        points3(iang3, 2) = -90.0e0_C_DOUBLE                                    ! [°]
        iang3 = iang3 + 1_C_LONG_LONG

        ! Add the point after it crosses the South Pole ...
        points3(iang3, 1) = endLon2                                             ! [°]
        points3(iang3, 2) = endLat2                                             ! [°]
        iang3 = iang3 + 1_C_LONG_LONG

        ! Fill in the second-half of the points (missing out 9 o'clock and the
        ! second 12 o'clock to make space for the three new points) ...
        DO iang2 = midang + 1_C_LONG_LONG, nang - 1_C_LONG_LONG
            IF(iang2 == lasang)THEN
                CYCLE
            END IF
            points3(iang3, 1) = ring(iang2, 1)                                  ! [°]
            points3(iang3, 2) = ring(iang2, 2)                                  ! [°]
            iang3 = iang3 + 1_C_LONG_LONG
        END DO

        ! Overwrite the points ...
        DO iang = 1_C_LONG_LONG, nang
            ring(iang, 1) = points3(iang, 1)                                    ! [°]
            ring(iang, 2) = points3(iang, 2)                                    ! [°]
        END DO

        ! Deallocate array ...
        DEALLOCATE(points3)

        ! Return ...
        RETURN
    END IF
END SUBROUTINE buffer_point_crudely

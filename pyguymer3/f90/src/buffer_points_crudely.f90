SUBROUTINE buffer_points_crudely(points1, dist, nang, npoint, points2, tol)
    ! Import standard modules ...
    USE ISO_C_BINDING

    ! Import my modules ...
    USE mod_safe, ONLY: sub_calc_loc_from_loc_and_bearing_and_dist

    IMPLICIT NONE

    ! Declare inputs/outputs ...
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: nang
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: npoint
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: dist
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: tol
    REAL(kind = C_DOUBLE), DIMENSION(npoint, 2), INTENT(in)                     :: points1
    REAL(kind = C_DOUBLE), DIMENSION(npoint, nang, 2), INTENT(out)              :: points2

    ! Declare internal variables ...
    INTEGER(kind = C_LONG_LONG)                                                 :: firang
    INTEGER(kind = C_LONG_LONG)                                                 :: iang
    INTEGER(kind = C_LONG_LONG)                                                 :: iang2
    INTEGER(kind = C_LONG_LONG)                                                 :: iang3
    INTEGER(kind = C_LONG_LONG)                                                 :: ipoint
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
    !$omp private(ipoint)                                                       &
    !$omp shared(dist)                                                          &
    !$omp shared(nang)                                                          &
    !$omp shared(npoint)                                                        &
    !$omp shared(points1)                                                       &
    !$omp shared(points2)
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
                ! Calculate initial angle ...
                ang1 = 360.0e0_C_DOUBLE * REAL(iang - 1_C_LONG_LONG, kind = C_DOUBLE) / REAL(nang - 1_C_LONG_LONG, kind = C_DOUBLE)

                ! Loop over points ...
                DO ipoint = 1_C_LONG_LONG, npoint
                    ! Calculate the ring coordinates and add them to the array ...
                    CALL sub_calc_loc_from_loc_and_bearing_and_dist(points1(ipoint, 1), points1(ipoint, 2), ang1, dist, points2(ipoint, iang, 1), points2(ipoint, iang, 2), ang2)
                END DO
            END DO
        !$omp end do
    !$omp end parallel

    !$omp parallel                                                              &
    !$omp default(none)                                                         &
    !$omp private(ang2)                                                         &
    !$omp private(dlon1)                                                        &
    !$omp private(dlon3)                                                        &
    !$omp private(endLat1)                                                      &
    !$omp private(endLat2)                                                      &
    !$omp private(endLon1)                                                      &
    !$omp private(endLon2)                                                      &
    !$omp private(iang2)                                                        &
    !$omp private(iang3)                                                        &
    !$omp private(ipoint)                                                       &
    !$omp private(points3)                                                      &
    !$omp shared(dist)                                                          &
    !$omp shared(firang)                                                        &
    !$omp shared(lasang)                                                        &
    !$omp shared(midang)                                                        &
    !$omp shared(nang)                                                          &
    !$omp shared(npoint)                                                        &
    !$omp shared(points1)                                                       &
    !$omp shared(points2)                                                       &
    !$omp shared(tol)
        !$omp do                                                                &
        !$omp schedule(dynamic)
            ! Loop over points ...
            DO ipoint = 1_C_LONG_LONG, npoint
                ! Create short-hand ...
                dlon1 = ABS(points1(ipoint, 1) - points2(ipoint, 1, 1))         ! [°]

                ! Check if it goes over the North Pole ...
                IF(dlon1 >= tol)THEN
                    ! Allocate array ...
                    ALLOCATE(points3(nang, 2))                                  ! [°]

                    ! Fix the first point ...
                    points3(1, 1) = points2(ipoint, 1, 1)                       ! [°]
                    points3(1, 2) = 90.0e0_C_DOUBLE                             ! [°]

                    ! Fix the last point ...
                    points3(nang, 1) = points2(ipoint, nang, 1)                 ! [°]
                    points3(nang, 2) = 90.0e0_C_DOUBLE                          ! [°]

                    ! Fill in the first-half and the second-half of the points
                    ! (missing out 3 o'clock and 9 o'clock to make space for the
                    ! two new points) ...
                    iang3 = 2_C_LONG_LONG
                    DO iang2 = 1_C_LONG_LONG, nang
                        IF(iang2 == firang .OR. iang2 == lasang)THEN
                            CYCLE
                        END IF
                        points3(iang3, 1) = points2(ipoint, iang2, 1)           ! [°]
                        points3(iang3, 2) = points2(ipoint, iang2, 2)           ! [°]
                        iang3 = iang3 + 1_C_LONG_LONG
                    END DO

                    ! Overwrite the points ...
                    DO iang = 1_C_LONG_LONG, nang
                        points2(ipoint, iang, 1) = points3(iang, 1)             ! [°]
                        points2(ipoint, iang, 2) = points3(iang, 2)             ! [°]
                    END DO

                    ! Deallocate array ...
                    DEALLOCATE(points3)

                    ! Skip ...
                    CYCLE
                END IF

                ! Create short-hand ...
                dlon3 = ABS(points1(ipoint, 1) - points2(ipoint, midang, 1))    ! [°]

                ! Check if it goes over the South Pole ...
                if(dlon3 >= tol)THEN
                    ! Allocate array ...
                    ALLOCATE(points3(nang, 2))                                  ! [°]

                    ! Fill in the first-half of the points (missing out 3
                    ! o'clock to make space for the three new points) ...
                    iang3 = 1_C_LONG_LONG
                    DO iang2 = 1_C_LONG_LONG, midang - 1_C_LONG_LONG
                        IF(iang2 == firang)THEN
                            CYCLE
                        END IF
                        points3(iang3, 1) = points2(ipoint, iang2, 1)           ! [°]
                        points3(iang3, 2) = points2(ipoint, iang2, 2)           ! [°]
                        iang3 = iang3 + 1_C_LONG_LONG
                    END DO

                    ! Replace the single point with a pair of points 0.005°
                    ! either side of it (this is required because the point is
                    ! not duplicated, unlike at the North Pole) ...
                    CALL sub_calc_loc_from_loc_and_bearing_and_dist(points1(ipoint, 1), points1(ipoint, 2), 179.995e0_C_DOUBLE, dist, endLon1, endLat1, ang2)
                    CALL sub_calc_loc_from_loc_and_bearing_and_dist(points1(ipoint, 1), points1(ipoint, 2), 180.005e0_C_DOUBLE, dist, endLon2, endLat2, ang2)

                    ! Add the point before it crosses the South Pole ...
                    points3(iang3, 1) = endLon1                                 ! [°]
                    points3(iang3, 2) = endLat1                                 ! [°]
                    iang3 = iang3 + 1_C_LONG_LONG

                    ! Fix the point before it crosses the South Pole ...
                    points3(iang3, 1) = endLon1                                 ! [°]
                    points3(iang3, 2) = -90.0e0_C_DOUBLE                        ! [°]
                    iang3 = iang3 + 1_C_LONG_LONG

                    ! Fix the point after it crosses the South Pole ...
                    points3(iang3, 1) = endLon2                                 ! [°]
                    points3(iang3, 2) = -90.0e0_C_DOUBLE                        ! [°]
                    iang3 = iang3 + 1_C_LONG_LONG

                    ! Add the point after it crosses the South Pole ...
                    points3(iang3, 1) = endLon2                                 ! [°]
                    points3(iang3, 2) = endLat2                                 ! [°]
                    iang3 = iang3 + 1_C_LONG_LONG

                    ! Fill in the second-half of the points (missing out 9
                    ! o'clock and the second 12 o'clock to make space for the
                    ! three new points) ...
                    DO iang2 = midang + 1_C_LONG_LONG, nang - 1_C_LONG_LONG
                        IF(iang2 == lasang)THEN
                            CYCLE
                        END IF
                        points3(iang3, 1) = points2(ipoint, iang2, 1)           ! [°]
                        points3(iang3, 2) = points2(ipoint, iang2, 2)           ! [°]
                        iang3 = iang3 + 1_C_LONG_LONG
                    END DO

                    ! Overwrite the points ...
                    DO iang = 1_C_LONG_LONG, nang
                        points2(ipoint, iang, 1) = points3(iang, 1)             ! [°]
                        points2(ipoint, iang, 2) = points3(iang, 2)             ! [°]
                    END DO

                    ! Deallocate array ...
                    DEALLOCATE(points3)

                    ! Skip ...
                    CYCLE
                END IF
            END DO
        !$omp end do
    !$omp end parallel
END SUBROUTINE buffer_points_crudely

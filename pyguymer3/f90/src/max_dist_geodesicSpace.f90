SUBROUTINE max_dist_geodesicSpace(                                              &
    n,                                                                          &
    midLon,                                                                     &
    midLat,                                                                     &
    lons,                                                                       &
    lats,                                                                       &
    maxDist,                                                                    &
    eps,                                                                        &
    nMax                                                                        &
)
    !f2py threadsafe

    ! Import standard modules ...
    USE ISO_C_BINDING

    ! Import my modules ...
    USE mod_safe, ONLY: sub_max_dist_geodesicSpace

    IMPLICIT NONE

    ! Declare input variables/outputs ...
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: n
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: midLon
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: midLat
    REAL(kind = C_DOUBLE), DIMENSION(n), INTENT(in)                             :: lons
    REAL(kind = C_DOUBLE), DIMENSION(n), INTENT(in)                             :: lats
    REAL(kind = C_DOUBLE), INTENT(out)                                          :: maxDist

    ! Declare optional input variables/outputs ...
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: nMax
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: eps

    ! **************************************************************************

    ! Call actual subroutine ...
    CALL sub_max_dist_geodesicSpace(                                            &
              n = n,                                                            &
         midLon = midLon,                                                       &
         midLat = midLat,                                                       &
           lons = lons,                                                         &
           lats = lats,                                                         &
        maxDist = maxDist,                                                      &
            eps = eps,                                                          &
           nMax = nMax                                                          &
    )
END SUBROUTINE max_dist_geodesicSpace

SUBROUTINE max_dist_euclideanSpace(                                             &
    n,                                                                          &
    midLon,                                                                     &
    midLat,                                                                     &
    lons,                                                                       &
    lats,                                                                       &
    maxDist                                                                     &
)
    !f2py threadsafe

    ! Import standard modules ...
    USE ISO_C_BINDING

    ! Import my modules ...
    USE mod_safe, ONLY: sub_max_dist_euclideanSpace

    IMPLICIT NONE

    ! Declare input variables/outputs ...
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: n
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: midLon
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: midLat
    REAL(kind = C_DOUBLE), DIMENSION(n), INTENT(in)                             :: lons
    REAL(kind = C_DOUBLE), DIMENSION(n), INTENT(in)                             :: lats
    REAL(kind = C_DOUBLE), INTENT(out)                                          :: maxDist

    ! **************************************************************************

    ! Call actual subroutine ...
    CALL sub_max_dist_euclideanSpace(                                           &
              n = n,                                                            &
         midLon = midLon,                                                       &
         midLat = midLat,                                                       &
           lons = lons,                                                         &
           lats = lats,                                                         &
        maxDist = maxDist                                                       &
    )
END SUBROUTINE max_dist_euclideanSpace

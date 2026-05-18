SUBROUTINE find_middle_of_locs_euclideanBox(                                    &
    n,                                                                          &
    lons,                                                                       &
    lats,                                                                       &
    midLon,                                                                     &
    midLat,                                                                     &
    maxDist                                                                     &
)
    !f2py threadsafe

    ! Import standard modules ...
    USE ISO_C_BINDING

    ! Import my modules ...
    USE mod_safe, ONLY: sub_find_middle_of_locs_euclideanBox

    IMPLICIT NONE

    ! Declare input variables/outputs ...
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: n
    REAL(kind = C_DOUBLE), DIMENSION(n), INTENT(in)                             :: lons
    REAL(kind = C_DOUBLE), DIMENSION(n), INTENT(in)                             :: lats
    REAL(kind = C_DOUBLE), INTENT(out)                                          :: midLon
    REAL(kind = C_DOUBLE), INTENT(out)                                          :: midLat
    REAL(kind = C_DOUBLE), INTENT(out)                                          :: maxDist

    ! **************************************************************************

    ! Call actual subroutine ...
    CALL sub_find_middle_of_locs_euclideanBox(                                  &
              n = n,                                                            &
           lons = lons,                                                         &
           lats = lats,                                                         &
         midLon = midLon,                                                       &
         midLat = midLat,                                                       &
        maxDist = maxDist                                                       &
    )
END SUBROUTINE find_middle_of_locs_euclideanBox

SUBROUTINE find_middle_of_locs_euclideanCircle(                                 &
    n,                                                                          &
    lons,                                                                       &
    lats,                                                                       &
    midLon,                                                                     &
    midLat,                                                                     &
    maxDist,                                                                    &
    angConv,                                                                    &
    debug,                                                                      &
    dist,                                                                       &
    nAng,                                                                       &
    nAngIter,                                                                   &
    nDistIter,                                                                  &
    nRefine                                                                     &
)
    !f2py threadsafe

    ! Import standard modules ...
    USE ISO_C_BINDING
    USE ISO_FORTRAN_ENV

    ! Import my modules ...
    USE mod_safe, ONLY: sub_find_middle_of_locs_euclideanCircle

    IMPLICIT NONE

    ! Declare input variables/outputs ...
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: n
    REAL(kind = C_DOUBLE), DIMENSION(n), INTENT(in)                             :: lons
    REAL(kind = C_DOUBLE), DIMENSION(n), INTENT(in)                             :: lats
    REAL(kind = C_DOUBLE), INTENT(out)                                          :: midLon
    REAL(kind = C_DOUBLE), INTENT(out)                                          :: midLat
    REAL(kind = C_DOUBLE), INTENT(out)                                          :: maxDist

    ! Declare optional input variables/outputs ...
    LOGICAL, INTENT(in)                                                         :: debug
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: nAng
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: nAngIter
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: nDistIter
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: nRefine
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: angConv
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: dist

    ! **************************************************************************

    ! Call actual subroutine ...
    CALL sub_find_middle_of_locs_euclideanCircle(                               &
                n = n,                                                          &
             lons = lons,                                                       &
             lats = lats,                                                       &
           midLon = midLon,                                                     &
           midLat = midLat,                                                     &
          maxDist = maxDist,                                                    &
          angConv = angConv,                                                    &
            debug = LOGICAL(debug, kind = INT8),                                &
             dist = dist,                                                       &
          iRefine = 1_INT64,                                                    &
             nAng = nAng,                                                       &
         nAngIter = nAngIter,                                                   &
        nDistIter = nDistIter,                                                  &
          nRefine = nRefine                                                     &
    )
END SUBROUTINE find_middle_of_locs_euclideanCircle

SUBROUTINE find_middle_of_locs_geodesicCircle(                                  &
    n,                                                                          &
    lons,                                                                       &
    lats,                                                                       &
    midLon,                                                                     &
    midLat,                                                                     &
    maxDist,                                                                    &
    angConv,                                                                    &
    debug,                                                                      &
    dist,                                                                       &
    eps,                                                                        &
    nAng,                                                                       &
    nAngIter,                                                                   &
    nDistIter,                                                                  &
    nMax,                                                                       &
    nRefine                                                                     &
)
    !f2py threadsafe

    ! Import standard modules ...
    USE ISO_C_BINDING
    USE ISO_FORTRAN_ENV

    ! Import my modules ...
    USE mod_safe, ONLY: sub_find_middle_of_locs_geodesicCircle

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
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: nMax
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: nRefine
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: angConv
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: dist
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: eps

    ! **************************************************************************

    ! Call actual subroutine ...
    CALL sub_find_middle_of_locs_geodesicCircle(                                &
                n = n,                                                          &
             lons = lons,                                                       &
             lats = lats,                                                       &
           midLon = midLon,                                                     &
           midLat = midLat,                                                     &
          maxDist = maxDist,                                                    &
          angConv = angConv,                                                    &
            debug = LOGICAL(debug, kind = INT8),                                &
             dist = dist,                                                       &
              eps = eps,                                                        &
          iRefine = 1_INT64,                                                    &
             nAng = nAng,                                                       &
         nAngIter = nAngIter,                                                   &
        nDistIter = nDistIter,                                                  &
             nMax = nMax,                                                       &
          nRefine = nRefine                                                     &
    )
END SUBROUTINE find_middle_of_locs_geodesicCircle

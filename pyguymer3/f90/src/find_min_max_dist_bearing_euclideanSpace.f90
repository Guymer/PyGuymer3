SUBROUTINE find_min_max_dist_bearing_euclideanSpace(                            &
    n,                                                                          &
    midLon,                                                                     &
    midLat,                                                                     &
    lons,                                                                       &
    lats,                                                                       &
    bestAng,                                                                    &
    angConv,                                                                    &
    angHalfRange,                                                               &
    debug,                                                                      &
    dist,                                                                       &
    nAng,                                                                       &
    nAngIter,                                                                   &
    nDistIter,                                                                  &
    startAng                                                                    &
)
    !f2py threadsafe

    ! Import standard modules ...
    USE ISO_C_BINDING
    USE ISO_FORTRAN_ENV

    ! Import my modules ...
    USE mod_safe, ONLY: sub_find_min_max_dist_bearing_euclideanSpace

    IMPLICIT NONE

    ! Declare input variables/outputs ...
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: n
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: midLon
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: midLat
    REAL(kind = C_DOUBLE), DIMENSION(n), INTENT(in)                             :: lons
    REAL(kind = C_DOUBLE), DIMENSION(n), INTENT(in)                             :: lats
    REAL(kind = C_DOUBLE), INTENT(out)                                          :: bestAng

    ! Declare optional input variables/outputs ...
    LOGICAL, INTENT(in)                                                         :: debug
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: nAng
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: nAngIter
    INTEGER(kind = C_LONG_LONG), INTENT(in)                                     :: nDistIter
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: angConv
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: angHalfRange
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: dist
    REAL(kind = C_DOUBLE), INTENT(in)                                           :: startAng

    ! **************************************************************************

    ! Call actual subroutine ...
    CALL sub_find_min_max_dist_bearing_euclideanSpace(                          &
                   n = n,                                                       &
              midLon = midLon,                                                  &
              midLat = midLat,                                                  &
                lons = lons,                                                    &
                lats = lats,                                                    &
             bestAng = bestAng,                                                 &
             angConv = angConv,                                                 &
        angHalfRange = angHalfRange,                                            &
               debug = LOGICAL(debug, kind = INT8),                             &
                dist = dist,                                                    &
            iAngIter = 1_INT64,                                                 &
           iDistIter = 1_INT64,                                                 &
             iRefine = 1_INT64,                                                 &
                nAng = nAng,                                                    &
            nAngIter = nAngIter,                                                &
           nDistIter = nDistIter,                                               &
             nRefine = 1_INT64,                                                 &
            startAng = startAng                                                 &
    )
END SUBROUTINE find_min_max_dist_bearing_euclideanSpace

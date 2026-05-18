MODULE funcs
    IMPLICIT NONE

    CONTAINS

    ! Include functions and subroutines ...
    INCLUDE "src/add.f90"
    INCLUDE "src/buffer_point_crudely.f90"
    INCLUDE "src/buffer_points_crudely.f90"
    INCLUDE "src/find_middle_of_locs_euclideanBox.f90"
    INCLUDE "src/find_middle_of_locs_euclideanCircle.f90"
    INCLUDE "src/find_middle_of_locs_geodesicCircle.f90"
END MODULE funcs

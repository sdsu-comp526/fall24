PROGRAM Area
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE Compute_Area(r, Area)

            REAL, INTENT(IN) :: r
            REAL, INTENT(OUT) :: Area

        END SUBROUTINE Compute_Area
    END INTERFACE

    REAL :: radius, Area_Circle

    write(*, '(A)', ADVANCE = "NO") "Enter radius of circle: "
    read(*,*) radius

    CALL Compute_Area(radius, Area_Circle)

    ! Write out area of circle using function call
    write(*, 100) "Area of circle with radius", radius, " is", Area_Circle
100 FORMAT(A, 2x, F6.2, A, 2x, F11.2) ! Need Fortran90 specification, i.e., name your file with the .f90 extension
  END PROGRAM Area

  SUBROUTINE Compute_Area(r, Area)

    IMPLICIT NONE
    REAL, INTENT(IN) :: r
    REAL, INTENT(OUT) :: Area
    REAL, PARAMETER :: Pi = 3.1415927

    Area = Pi*r*r

  END SUBROUTINE Compute_Area

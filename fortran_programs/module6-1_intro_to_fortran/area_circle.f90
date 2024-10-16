      PROGRAM Area
        IMPLICIT NONE

        INTERFACE

            FUNCTION Area_Circle(r)

            REAL :: Area_Circle

            REAL, INTENT(IN) :: r

            END FUNCTION Area_Circle
        END INTERFACE

        REAL :: radius

        write(*, '(A)', ADVANCE = "NO") "Enter radius of circle: "
        read(*,*) radius

        ! Write out area of circle using function call
        write(*, 100) "Area of circle with radius", radius, " is", Area_Circle(radius)
100 FORMAT(A, 2x, F6.2, A, 2x, F11.2) ! Need Fortran90 specification, i.e., name your file with the .f90 extension
      END PROGRAM Area

      FUNCTION Area_Circle(r)

        IMPLICIT NONE
        REAL :: Area_Circle
        REAL, INTENT (IN) :: r

        REAL, PARAMETER :: Pi = 3.1415927

        Area_Circle = Pi*r*r

      END FUNCTION Area_Circle

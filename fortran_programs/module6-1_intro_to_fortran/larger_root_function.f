      REAL FUNCTION LargerRoot(a,b,c)

        IMPLICIT NONE ! Remember that this tells the compiler that we will define all variables we will use

        REAL, INTENT(IN) :: a
        REAL, INTENT(IN) :: b
        REAL, INTENT(IN) :: c

        REAL             :: d, r1,r2 ! Some temporary auxiliary variables needed that will live only inside this function

        d = SQRT(b*b - 4.0*a*c)

        r1 = (-b+d) / (2.0*a)

        r2 = (-b-d) / (2.0*a)

        IF(r1 >= r2) THEN
          LargerRoot = r1
        ELSE
          LargerRoot = r2
        END IF

      END FUNCTION LargerRoot

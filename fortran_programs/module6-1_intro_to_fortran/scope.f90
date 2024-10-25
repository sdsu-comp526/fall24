    PROGRAM Scope
        IMPLICIT NONE
        INTEGER :: a = 1, b = 2, c = 3 ! a,b,c are global

        WRITE (*,*) Add(a) ! a = 4
        c = 4
        WRITE (*,*) Add(a) ! a = 5
        WRITE (*,*) Mul (b,c)

    CONTAINS
        INTEGER FUNCTION Add(q)
            IMPLICIT NONE
            INTEGER , INTENT (IN) :: q

            Add = q + c ! c is used here, from global scope
        END FUNCTION Add

        INTEGER FUNCTION Mul(x,y)
            IMPLICIT NONE

            INTEGER, INTENT (IN) :: x,y

            Mul = x * y
        END FUNCTION Mul
    END PROGRAM Scope

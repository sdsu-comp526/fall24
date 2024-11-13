PROGRAM lagrange_program
    ! this is the main program that will call the subroutine
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE lagrange_interpolation(x_data,y_data,x,y)

            REAL, dimension(:),intent(in) :: x_data
            REAL, dimension(:),intent(in) :: y_data
            REAL, dimension(:),intent(in) :: x
            REAL, dimension(:),intent(out) :: y

        END SUBROUTINE lagrange_interpolation
    END INTERFACE

    INTEGER, parameter :: n = 5 ! number of data points
    INTEGER, parameter :: s = 50 ! number of domain points for the function approximation
    INTEGER :: i
    REAL :: x_data(n), y_data(n)
    REAL :: x(s), y(s)

    OPEN(1, file = 'input_data.txt', ACTION='READ')

    WRITE(*,*) 'Reading input data from file' ! this prints out on the screen

    do i = 1, n
        READ(1,100)x_data(i), y_data(i) ! read the data using the 100 Format specified below
    end do

    WRITE(*,*) 'Writing input data to screen' ! this prints out on the screen
    do i = 1, n
        WRITE(*,100)x_data(i), y_data(i) ! write the data using the 100 Format specified below
    end do

    WRITE(*,*) 'Reading domain data from file' ! this prints out on the screen
    OPEN(2, file = 'domain_points.txt', ACTION='READ')
    do i = 1, s
        READ(2,101)x(i) ! read the data using the 101 Format specified below
    end do

    CLOSE(1)
    CLOSE(2)
    CALL flush(1)
    CALL flush(2)

    y = 0.0 ! initialize the output array
    WRITE(*,*) 'Call lagrange_interpolation subroutine' ! this prints out on the screen
    CALL lagrange_interpolation(x_data,y_data,x,y)

    WRITE(*,*) 'Writing data to output file' ! this prints out on the screen
    OPEN(3, file = 'output.txt')
    do i = 1, s
        WRITE(3,102)y(i) ! write the data using the 102 Format specified below
    end do

    CLOSE(3)
    CALL flush(3)


    100 FORMAT(F3.1, F6.3) ! Format specifier for two floating point values: one 3 columns wide, with 1 decimal place, and the other one 6 columns wide (counting from the end of the previous one), with 3 decimal places
    101 FORMAT(F19.17) ! Floating point format specifier 19 columns wide, with 17 decimal places
    102 FORMAT(F0.17) ! the 0 here means that processor selects the smallest positive field width necessary

END PROGRAM lagrange_program


SUBROUTINE lagrange_interpolation(x_data,y_data,x,p)

    ! This subroutine interpolates data points x_data=[x1,x2,...xn] and y_data=[f(x1),f(x2),...,f(xn)]
    ! Defines the Lagrange interpolating polynomial P(x) using Lagrange's interpolating polynomial formula over a domain of x points and
    ! Returns p = P(x)

    IMPLICIT NONE

    REAL, dimension(:),intent(in) :: x_data
    REAL, dimension(:),intent(in) :: y_data
    REAL, dimension(:),intent(in) :: x
    REAL, dimension(:),intent(out) :: p

    !local variables:
    INTEGER :: i,j,n,m
    INTEGER, parameter :: s = 50 ! instead of redefining this here, we could have defined in a module and imported it here with the USE statement

    REAL :: Li(s)

    !check number of points:
    n = size(x_data)
    m = size(y_data)
    if (n/=m) error stop &
        'Error: data point vectors must be the same size.'

    do i=1,n
        Li = 1.0 ! initialize the L variable for the Lagrange interpolant
        do j=1,n
            if (i/=j) Li = Li * (x-x_data(j)) / (x_data(i)-x_data(j))
        end do
        p = p + Li*y_data(i)
    end do
END SUBROUTINE lagrange_interpolation

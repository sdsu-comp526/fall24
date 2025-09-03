# Assignment 3:

## 🤓 Assignment overview and learning outcomes

This assignment will cover some of the topics of the Fortran programming language seen in class, such as some basic I/O.

We will revist here an interpolation method seen in class, [Lagrange's interpolation method](https://sdsu-comp526.github.io/fall24/slides/module7-1_interpolation.html), and we will learn how to use Lagrange’s interpolating formula to approximate a function $f(x)$ over a domain $[a,b]$.

## 📝 Assignment steps:
1. Write your own Fortran program, in a file called `main_lagrange.f90`, to find the interpolating polynomial for the following data points $(x_i,y_i)$: $(0.0, 1.0)$, $(0.5, 0.223)$, $(1.0, 0.05)$, $(1.5, 0.011)$, $(2.0, 0.002)$. The abscissae $x_i$ are defined to be 5 equi-spaced points on the domain $[a,b] = [0,2]$, and the corresponding ordinates $y_i$ are defined as $y_i \equiv f(x_i) = e^{-3 x_i}$ .
2. (20%) In your program, you need to read the data points from the input file provided, called `input_data.txt`, and properly store their values in arrays called `x_data` and `y_data` (one for the abscissae and one for the ordinates, respectively).
3. (10%) Write these input data points to standard output (i.e., your screen) to make sure that you have read and formatted the input values correctly.
4. (20%) Read the domain points (which are 50 equi-spaced points in $[a,b]$) provided in the file called `domain_points.txt` and properly store their values in an array called `x`.
5. (30%) Write your lagrange interpolation in a Fortran subroutine with the following `INTERFACE`:
    ```fortran
        INTERFACE
            SUBROUTINE lagrange_interpolation(x_data,y_data,x,y)

                REAL, dimension(:),intent(in) :: x_data
                REAL, dimension(:),intent(in) :: y_data
                REAL, dimension(:),intent(in) :: x
                REAL, dimension(:),intent(out) :: y

            END SUBROUTINE lagrange_interpolation
        END INTERFACE
    ```
    and call this subroutine from your main program
6. (20%) Finally, write the output $y$ from your `lagrange_interpolation` subroutine in a file called `output.txt`.
7. Other requirements and specs for your Fortran program (i.e., a non-exhaustive list of things that might get you points off):
    a. Forgetting to use `IMPLICIT NONE` or using it improperly (-20%)
    b. Misuses of or changes in the interface (-10% each)
8. **Extra Credit (+15%)**: use Julia to plot the approximated polynomial, the original function $f(x) = e^{-3x}$, and the discrete data points $(x_i,y_i)$ (all of these overlayed on the same plot). Save the generated plot to file, with a `png` file format.


## Submission requirements:
Follow the [submission expectations](https://sdsu-comp526.github.io/fall24/slides/module4-6_review.html#submission-expectations) that we covered in the Review lecture in class. Mainly, you are required to work on a _feature_ branch in your assignment repository (do NOT work off `main`), push your `main_lagrange.f90` and `output.txt` files, and any accompanying files for the Extra Credit question (if you decide to complete it) in an open Pull Request. Always test your program compilation and execution locally, before submitting it.

Do **not** close or merge your Pull Request. Leave it open so that your teacher can Review it.

If you decide to complete the EC question, follow the [submission expectations for assignments in Julia](https://sdsu-comp526.github.io/fall24/slides/module4-6_review.html#assignments-in-julia), mainly, submit your julia file with the `.jl` extension, the `Project.toml` and `Manifest.toml` file.

## Code skeleton and hints:

Here is some Fortran pseudo-code that will help you get started:

```fortran

    PROGRAM lagrange_program

    ! Specify interface of subroutine here

    ! Define parameters and arrays here

    ! I/O requested in Steps 2, 3, and 4 of the Assignment Steps

    CALL lagrange_interpolation(x_data,y_data,x,y)

    ! I/O requested in Step 6 of the Assignment Steps

    100 FORMAT(F3.1, F6.3) ! Format specifier for two floating point values: one 3 columns wide, with 1 decimal place, and the other one 6 columns wide (counting from the end of the previous one), with 3 decimal places
    101 FORMAT(F19.17) ! Floating point format specifier 19 columns wide, with 17 decimal places
    102 FORMAT(F0.17) ! the 0 here means that processor selects the smallest positive field width necessary

    END PROGRAM lagrange_program

    SUBROUTINE lagrange_interpolation(x_data,y_data,x,p)

    ! your subroutine code here

    END SUBROUTINE lagrange_interpolation
```

### Hints:
**I/O in Julia**: for the EC question, you will need the following block of code:

```julia
# open output.txt data and store values in the array called p
open("output.txt","r") do f
    line = 1
    while ! eof(f)
        l = readline(f)
        p[line] = parse.(Float64,l)
        line += 1
    end
end
```

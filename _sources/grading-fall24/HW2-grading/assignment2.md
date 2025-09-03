# Assignment 2:

## 🤓 Assignment overview and learning outcomes

This assignment will cover some of the topics of the C programming language seen in class, such as some basic data types, pointers, and the different C memory pools.

We will revist here a root-finding method seen in class: the Bisection method.

## 📝 Assignment steps:
1. Write your own C program, in a file called `bisection.c`, to find the roots, $x_{\star}$, of $e^{-x}(3.2\sin(x) - 0.5 \cos(x)) = 3$ with the bisection method, within a $10^{-4}$ tolerance, for the following intervals:
  a. (10%) $[a_1,b_1] = [-4, -2]$
  b. (10%) $[a_2,b_2] = [2, 4]$
  c. (10%) Make sure to save the history of all approximated solutions, and (10%) the total number of iterations your algorithm performed.
2. Has your bisection method found the root in the given interval? Use the `printf()` function to print the following information:
  a. (10%) The value of the found $x_{\star}$ and check that that's a root by printing the value of $f(x_{\star})$.
  b. (10%) How many iterations your algorithm took to find the root.
  c. (10%) If no root was found, your program should print an error message saying why it was not found.
3. Make use of all three C pools of memory seen in class:
  a. (10%) Define the endpoints of your intervals `a1, b1, a2, b2` in the static pool of memory.
  b. (10%) Define at least the variables for the number of iterations and the desired tolerance in the stack memory.
  c. (10%) Define the arrays/pointers needed to store all approximated solutions in the heap memory.
4. Other requirements and specs for your C program:
  a. It should not use recursion. (-20%)
  b. It should not have memory leaks. (-10% per memory leak)
5. Extra Credit: (+15%)
  a. Compute the relative condition number, $$ \kappa = |f'(x)| \frac{|x|}{|f|} . $$  and plot it as a function of $x$.
  b. Is the function well/ill-conditioned $\forall x \in [a,b]$ (for both cases of intervals given in questions `1.a` and `1.b`.)?

## Submission requirements:
You are required to work on a branch in your assignment repository, push all of your work, that is, your C program in the `bisection.c` file and any accompanying files for the Extra Credit question (if you decide to complete it) in an open Pull Request. Always test your program compilation and execution locally, before submitting it.

For the Extra Credit question, you can use Julia, MATLAB, Python, or even calculations by hand. Please push/upload to the same Pull Request all of the supporting documents showing all of your work: e.g., your Julia, MATLAB, or Python code, the generated plot (saved in a `png` file format) and/or any supporting hand calculations (if you decide to include hand written calculations, take a photo of your handwritten calculations and push it or drop it in the PR conversation.)


## Code skeleton and hints:

Here is some pseudo-code that will help you get started:

```c

#include <stdio.h>  // needed to be able to use printf
#include <stdlib.h> // needed to be able to define some variables in the heap
#include <math.h>   // needed to be able to use some math primitive functions, such as fabs() for absolute value of floating point numbers


// static memory variables defined here

int bisect_hist(double *hist, double a, double b, double tol); // signature/declaration of your bisection method function

// define the function f we want to find the roots of
double f(double x0){
    double x;
    // some code here
    return x;
}

int bisect_hist(double *hist, double a, double b, double tol) {

    // body of the bisect_hist function here

}

int main(void){

    // body of the main function here

}

```

Hints:
- When compiling your program, you want to also use the flag `-ml` to be able to include the standard math library defined in `<math.h>`. If you were to use `gcc` as a compiler, you would compile with the following:
```bash
gcc bisection.c -o bisection -lm
```
Or, if you are using the `clang` compiler, you would compile with the following:
```bash
clang bisection.c -o bisection -lm
```
Other compilers: Most C compilers use a similar flag. Refer to your compiler’s documentation if you are using a different one.
- When computing the length of the interval, $|a-b|$, use the C function for absolute values of floating points, [`fabs()`](https://en.cppreference.com/w/cpp/numeric/math/fabs). Do not use [`abs()`](https://en.cppreference.com/w/cpp/numeric/math/abs) or you'll get incorrect results!



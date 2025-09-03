# Assignment 4:

## 🤓 Assignment overview and learning outcomes

This assignment will cover some of the topics of numerical integration and quadrature seen in class. You will implement the Trapezoid and Simpson rules, and Gauss quadrature with both Legendre and Lobatto nodes. You will analyze their accuracy, and interpret the behavior of the errors observed.

### Trapezoid Rule

To approximate $I(f)= \int_a^b f(x) dx$ , let the number of subintervals to be $n$ and let

$$
h=\frac{b-a}{n}
$$

be the length of each subinterval. The endpoints (equispaced) of the subintervals are given by

$$
x_j=a + hj, \qquad j=0\, , \ldots \, , n
$$

Then the approimation of $I(f)$ by the Trapezoid rule is given by the formula

$$
T_n(f)=h\left[\frac{1}{2}f(x_0) + f(x_1) + f(x_2) + \ldots + f(x_{n-2}) + f(x_{n-1}) + \frac{1}{2}f(x_n) \right]
$$


### Simpson's Rule

For the same number of subintervals and grid points the Simpson's rule follows:

$$
S_n(x)= \frac{h}{3} \left[ f(x_0) + 4 f(x_1) + 2f(x_2) + 4 f(x_3) + 2f(x_4) + \ldots + 2f(x_{n-2}) + 4 f(x_{n-1}) + f(x_n)  \right]
$$


### Gauss Quadrature

We approximate

$$
\int_{-1}^{-1} f(x) dx \approx \sum_{i=1}^{n} w_i f(x_i)
$$

where $w_i$ are the weights and $x_i$ the nodes.


To approximate integrals on a general interval $[a,b]$, the problem needs to be translated to $[-1,1]$ with a simple change of variables. Using the substitution $t = (2x - a - b) / (b-a)$, we find

$$
\int_a^b f(x) dx = \int_{-1}^{1} f \left( \frac{(b-a)t + b + a}{2} \right)  \frac{(b-a)}{2} dt
$$

## 📝 Assignment steps and rubric:
1. (25%) Implement your own Trapezoid and Simpson rules, and Gauss quadrature in Julia.
2. (25%) Use the Trapezoid, Simpson, Gauss quadrature with both the Legendre (denoted by LG) and Lobatto (denoted by LGL) nodes to find the approximations $I_n(f)$ of integrals $I(f)=\int_a^b f(x)dx$, with $n=2^2, 2^3, \ldots , 2^9$, for the following:

$$
\begin{align}
(a)&  \int_0^\pi e^{x} \cos (4x) dx = \frac{e^\pi -1}{17}\\
(b)& \int_0^1 x^{5/2} dx= \frac{2}{7}\\
(c)& \int_0^5 \frac{1}{1+(x-\pi)^2} dx= \arctan(5 - \pi)+ \arctan(\pi) \\
(d)& \int_0^{\pi /4} {(e^{\cos x}) }dx = 1.93973485062365\\
(e)& \int_0^1 \sqrt{x} dx = \frac{2}{3}
\end{align}
$$

For each of the approximated integrals $I_n(f)$ calculate the error as the absolute value of the difference from the exact value of the integral $I(f)$, given by

$$
Err_n=| I(f) - I_n(f) |
$$

3. (25%) For each exercise $(a)-(e)$ print one table that in each row displays the results for each value of $n$. In each table, each row should have the number $n$ of points used, the approximation of the integral by Trapezoidal Rule, its error $Err_n$, and its ratio $Ratio_n=\frac{Err_{n-1}}{Err_n}$, respectively, then the approximation by Simpson's Rule, its error and its ratio, respectively, then the Gauss quadrature with Legendre points, its error and its ratio, respectively, then the Gauss quadrature with Lobatto points, its error and its ratio, respectively.
Hence, each table for  exercise $(a)-(e)$ should have the following headers:

`@printf("  n      Trap      Err_n_Trap  Ratio_n_Trap     Simpson    Err_n_Simps  Ratio_n_Simps     GL       Err_GL     Ratio_n_GL      LGL        Err_LGL     Ratio_n_LGL \n")`

4. (25%) Comment on your results. For each execution $(a)-(e)$, which method is better? And _why_?

## Extra Credit:
(+15%) Plot the accuracy of these methods as we have done in class, with the error on the $y$-axis and $n$ on the $x$-axis. Also add to the plot a few reference lines with slopes $n^{-1}$, $n^{-2}$, $n^{-3}$, $n^{-4}$.


## Hints and a non-exhaustive list of things that might get you points off
- To use `@printf` you need to use the `Printf` package.
- Do not write/tabulate quadrature nodes and weights by hand. You can obtain Gauss quadrature nodes and weights via the `FastGaussQuadrature` package as we have seen in class, both for `gausslegendre` (GL) and `gausslobatto` (LGL) rules.
- For readability, do not have misaligned column entries in your tables (use format specifiers wisely).

## Submission requirements:

Follow the [submission expectations](https://sdsu-comp526.github.io/fall24/slides/module4-6_review.html#submission-expectations) that we covered in the Review lecture in class. Mainly, you are required to work on a _feature_ branch in your assignment repository (do NOT work off `main`), push your `.jl` Julia source code and `Project.toml` and `Manifest.toml` files. Always test your program execution locally, before submitting it.

Do **not** close or merge your Pull Request. Leave it open so that your teacher can Review it.



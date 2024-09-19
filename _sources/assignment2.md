# Exercise 10.3:
 1. Write your own bisection method in C that takes a function, the endpoints of the interval, a given tolerance (as in `(f, a, b, tol)`) and that returns the root and how many iterations it took to find it.
 2. Find an interval of length $1$ with integer endpoints on which a solution to $x^3=9$ lies. Then use your bisection code starting with this interval to find the solution within a $10^{-4}$ tolerance.
 3. Find an interval of length $1$ with integer endpoints on which a solution to $6 + \cos^2{x} = x$ lies. Then use your bisection code starting with this interval to find the solution within a $10^{-4}$ tolerance.
 4. Suppose we want to know what interest rate we would need to achieve (assuming constant interest rates) such that the initial deposit of $\$50,000$ and annual contribution of $\$10,000$ will grow to $\$1,000,000$ in $20$ years. Thus we need to solve $1000000 = \left( 50000 + \frac{10000}{r}  \right) e^{20r} - \frac{10000}{r}$ for the interest rate, $r$. Find a reasonable interval on which the solution ought to lie and use the bisection code to find $r$ to within $10^{-6}$.
 5. Use your bisection code to approximate a root of $f(x) = \frac{|x-2|}{x^3 - 2 x^2 + x -2}$ on the interval $[1, 4]$ with error no more than $10^{-4}$.
 6. Use your bisection code to approximate a root of $f(x) = \frac{|x-2|}{x^3 - 2 x^2 + x -2}$ on the interval $[4, 6]$ with error no more than $10^{-4}$.
 7. Find appropriate intervals and use your bisection code to find ALL the root(s) of $|x|e^{x}= 0.25$.
 8. Use your bisection code to find the root of $27 x^3 - 27 x^2 + 9x = 1$  on $[0, 1]$ to within $10^{-6}$.
 9. Use your bisection code to find the root of $64 x^3 - 48x^2 + 12x = 1$  on $[0, 1]$ to within $10^{-6}$.

 From module3-5_newton.ipynb:

 ### Exercise 12.2:
 1. Solve the fixed point iteration $g(x_n) = (2+{(x_n - 2)}^2)$, up to a tolerance $10^{-6}$ or maximum number of iterations $N=50$, with (a) $x_0 = 2.8$,  (b) $x_0 = 3.1$. Which execution is better? And why?
 2. Solve the fixed point iteration $g(x_n) = (2+{(x_n - 2)}^3)$, up to a tolerance $10^{-6}$ or maximum number of iterations $N=50$, with (a) $x_0 = 2.8$, (b) $x_0 = 3.1$. Which execution is better? And why?
 3. Use Newton's method to solve Exercise 10.3 points 1 and 3 from the Bisection method class. Which method performs better on these functions and why?

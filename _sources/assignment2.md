## Exercise 10.3:
 1. Find an interval of length $1$ with integer endpoints on which a solution to $x^3=9$ lies. Then use your bisection code starting with this interval to find the solution within a $10^{-4}$ tolerance.
 2. Find an interval of length $1$ with integer endpoints on which a solution to $6 + \cos^2{x} = x$ lies. Then use your bisection code starting with this interval to find the solution within a $10^{-4}$ tolerance.
 3. Suppose we want to know what interest rate we would need to achieve (assuming constant interest rates) such that the initial deposit of $\$50,000$ and annual contribution of $\$10,000$ will grow to $\$1,000,000$ in $20$ years. Thus we need to solve $1000000 = \left( 50000 + \frac{10000}{r}  \right) e^{20r} - \frac{10000}{r}$ for the interest rate, $r$. Find a reasonable interval on which the solution ought to lie and use the bisection code to find $r$ to within $10^{-6}$.
 4. Use your bisection code to approximate a root of $f(x) = \frac{|x-2|}{x^3 - 2 x^2 + x -2}$ on the interval $[1, 4]$ with error no more than $10^{-4}$.
 5. Use your bisection code to approximate a root of $f(x) = \frac{|x-2|}{x^3 - 2 x^2 + x -2}$ on the interval $[4, 6]$ with error no more than $10^{-4}$.
 6. Find appropriate intervals and use your bisection code to find ALL the root(s) of $|x|e^{x}= 0.25$.
 7. Use your bisection code to find the root of $27 x^3 - 27 x^2 + 9x = 1$  on $[0, 1]$ to within $10^{-6}$.
 8. Use your bisection code to find the root of $64 x^3 - 48x^2 + 12x = 1$  on $[0, 1]$ to within $10^{-6}$.

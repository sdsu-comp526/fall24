# COMP526 Midterm Project
## 🤓 Assignment overview and learning outcomes

This midterm project will cover some of the topics seen in class, such as the solution of systems of equations. Through this assignment you will learn how to implement your own stationary iterative solver in Julia and analyze its solution and performance by comparing your algorithm with the ones built-in the Julia package [IterativeSolvers.jl](https://iterativesolvers.julialinearalgebra.org/stable/#IterativeSolvers.jl).

You have the possibility of choosing among the following options:

1. [Jacobi method](https://en.wikipedia.org/wiki/Jacobi_method)

2. [Gauss-Seidel method](https://en.wikipedia.org/wiki/Gauss%E2%80%93Seidel_method)

3. [Successive over-relaxation (SOR)](https://en.wikipedia.org/wiki/Successive_over-relaxation) with a relaxation parameter $\omega = 1$.


## 📝 Assignment steps:
1. Choose a stationary iterative method of your choice among the options given above and implement it yourself in Julia to solve the matrix associated with the time-independent Poisson's problem:

$$
-\frac{\partial^2 u}{\partial x^2} = f(x)
$$

on a 1D domain $[a,b] = [-1,1]$ discretized with $m$ equispaced grid points, with right-hand side forcing function $f(x) =\pi^2 \sin(\pi x)$, and (homogeneous) Dirichelet boundary conditions:

$$
u(a) = 0, \; \quad u(b)=0 \;,
$$

which has the exact solution $u(x)=\sin(\pi x)$.

For the interior grid points, we have the corresponding equations for indices $i=2, \dots, m-1$:

$$
A = \frac{1}{\Delta x^2} \begin{bmatrix} 2 & -1 & 0 & & 0  \\ -1 & 2 & -1 & & \vdots  \\ 0 & -1 & 2 & & 0  \\ & & &  \ddots & -1 \\ 0 & \ldots & 0 & -1 & 2\end{bmatrix}_{(m-2) \times (m-2)}
$$

The first and last row of $A$ are going to be different, because of the Dirichlet boundary conditions. You have to derive yourself the discrete equations, for $i=1$ and $i=m$, and plug the correct coefficients in the first and last row of your matrix $A$.

The full system can then be written as:

$$
\begin{bmatrix}  & & & &   \\
                 & & & &   \\
                 & & A &   \\
                 & & & &   \\
                 & & & &
\end{bmatrix}_{m \times m}
\begin{bmatrix} u_1 \\ u_2 \\  \vdots \\  u_{m-1}\\ u_m
\end{bmatrix}_{m \times 1} =
\begin{bmatrix} f(x_1) \\ f(x_2) \\  \vdots \\  f(x_{m-1})\\ f(x_m)
\end{bmatrix}_{m \times 1}
$$

The stationary iterative methods need an initial guess for the iterates $u^{k+1}$. You can use $u^0 \equiv 0$ on all gridpoints of the domain.

Furthermore, your iterative method should perform repeated iterations until the residual at the $k$th-iteration, defined as:
$$
\textrm{residual}^k = b - A x^k
$$

is smaller than a tolerance (where $b$ is the right-hand side vector, $b=f(x)$, in this case). Use a tolerance of $10^{-5}$ for a stopping criterion.

2. Test it with a matrix $A \in \R^{m \times m}$ for:
  - a) $m=10^2$
  - b) $m=10^3$
  - c) $m=10^4$

3. Compare your results with the correspomding iterative scheme among the built-in Julia functions available in the  [IterativeSolvers.jl](https://iterativesolvers.julialinearalgebra.org/stable/#IterativeSolvers.jl) package. Also, compare with the left-division operator `\`, which performs a direct method.

4. Time your execution and the ones from the built-in Julia functions available in the [IterativeSolvers.jl](https://iterativesolvers.julialinearalgebra.org/stable/#IterativeSolvers.jl) package, and the left-division operator `\`, with the `@time` macro. (Hint: remember to use the macro `@time` twice, since the first time it might include precompilation times). How does your implementation compare with the ones in the Julia [IterativeSolvers.jl](https://iterativesolvers.julialinearalgebra.org/stable/#IterativeSolvers.jl) package? And how does it compare with the left-division operator `\`? Which one is best (in terms of running time and memory allocations)?

5. Present and discuss your work and results in a lightning presentation (**5 minutes only** per speaker!) in class. (You don't need to prepare a slide deck for your presentation. You can just share your screen, narrate the highlights of your code, execute it, and show your results). A calendar to sign up for the lightning presentation slots is going to be available at the following [link](https://app.simplymeet.me/valeriabarra/valeriabarra-comp526-midterm) around 10/20.

**In-class presentation dates: 10/28 (6 speakers) and 10/30 (6 speakers)**.

## Submission requirements and rubric:

Follow the [submission expectations](https://sdsu-comp526.github.io/fall24/slides/module4-6_review.html#submission-expectations) that we covered in the Review lecture in class.

The project grading is dependent on the following criteria:
1. Attempted difficulty.
2. Did you meet your major goals? The most important grading criterion is functionality: Having a running program is essential. A simpler solution that works will be graded much more favorably than an ambitious project that crashes (which cannot be executed, and therefore, graded).
3. The midterm project will be graded not only for your code and the answers to the questions, but also for the communication in your lightning talk:
(a) 60%: Project execution and analysis/comments on your findings.
(b) 40%: In class oral presentation and discussion.







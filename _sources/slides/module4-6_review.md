# 19) HW2 Review

## Last time
- Solving Systems
  - Direct methods
  - Iterative methods
  - Example of a PDE

## Today
- 1. Review of HW2
- 2. Submission expectations
  - 2.1 Assignments in Julia

## Review of Assignment 2

Here is my solution for the `bisection.c` code:

```{literalinclude} ../c_programs/module4-6_assignment2/bisection.c
:language: c
:linenos: false
```

### Common mistakes

Here is a list of common mistakes that a few people made and you should not make:

- Not using the absolute value to compute the length of the interval $[a,b]$. What if the values were complex? Distances (or lengths) are non-negative numbers by definition.

- Using `while ((b - a) / 2.0 > tol)` as a stopping criterion, effectively doubling the tolerance

- Using the midpoint of the interval `fabs(f(mid_point)) > tol` to check for the stopping criterion

- Not checking the necessary condition for the function to have a root in the interval. That is, not checking that it has opposite signs at the endpoints `f(a)*f(b)<0`

- Using `==` to check if a floating point number is equal to zero, as in `if (f(mid_point) == 0.0)`. You should _never_ use `==` to check if any floating point number is equal to zero, because of floating point arithmetic and rounding errors. Always check if it is within a desired tolerance. Alternatively, if you are not given a specific tolerance, check against machine $\varepsilon$, but do not check `== 0`.

## Submission expectations

- In general, when you receive an Assignment via a GitHub Classroom link, you want to clone your assignment repository, by doing

```shell
git clone your_assignment_repository_url
```

- You can also work on a back-up repository or directory of your choice if you want to, for your scrap work, but you _have to_ clone the assignment repository and submit your work there to be considered for submission and grading.

- As soon as you clone your Assignment repository, move to that repository

```bash
cd your_assignment_repository
```

- Create a new feature branch and switch to that. You can do this in two ways:
  * `git checkout -b name_of_your_branch`
  * `git branch name_of_your_branch` and then `git checkout name_of_your_branch`

- Do **NOT** work directly off `main`

- You can work on your feature branch as much as you like and create repeated incremental snapshots of your work via `git commit`. Always remember to use meaningful commit messages to remind yourself (and others) about your work in that moment in time. In a terminal you can simply do this by

```bash
git commit -m "Your commit message"
```

You can also write multi-line more detailed commit messages if you want. Just simply separate them with a space, and repeat the `-m` option, as in:

```bash
git commit -m "Your commit message" -m "Your more detailed message on a new line"
```

- When you are satisfied with your committed work, you can push it to your working branch via:

```bash
git push origin your_branch_name
```

If it is the first time you are doing this, `git` will automatically tell you that you can open a Pull Request with your changes. Just CTRL-click on the URL that git shows you in the terminal and you will be sent to your Pull Request web interface.

Any successive changes that you want to push to your branch, they will be automatically reflected on the open PR.

- Only changes made within the deadline (including the lateness window) will be graded.

- Remember not to attempt to close or merge your PR without any Reviewer (in this case your instructor) approval.

- Always remember to double check the `File changed` tab in your PR. If you see files that should not belong there (e.g., files automatically created by your IDE or virtual environment files) remove them.

- If you are using an IDE that automatically creates hidden project files that you might inadvertently push to your branch, it is always a good practice to use a `.gitignore` file that specify which files you do _not_ want to be tracked by `git`, and therefore, pushed to your branch. Recall that we covered this in our [first lecture](https://sdsu-comp526.github.io/fall24/slides/module1-1_first_class.html#keeping-track-with-git).

### 2.1 Assignments in Julia

- If you are asked to submit your code in Julia, recall what we have covered during our [first lecture](https://sdsu-comp526.github.io/fall24/slides/module1-1_first_class.html#julia).

  * You are required to submit not only your Julia code (i.e., the file with the `.jl` extension), but also the `Project.toml` and `Manifest.toml` files that are created when you instantiate your environment. These files will be automatically populated if you need to use any Julia package that is available in the Julia registry, meaning, any package that you add by doing

  ```julia
  ]add PackageName
  ```

  or in the Julia REPL:
  ```julia
  using Pkg
  Pkg.add("PackageName")
  ```

  These files are fundamental for your reviewer to be able to reproduce exactly your environment, run your code, and reproduce your results.


# 29) HW3 Solution

In this assignment we covered some of the topics of the Fortran programming language seen in class, such as some basic I/O.

We revisted an interpolation method seen in class, [Lagrange's interpolation method](https://sdsu-comp526.github.io/fall24/slides/module7-1_interpolation.html), and we learned how to _use_ Lagrange’s interpolating formula to approximate a function $f(x)$ over a domain $[a,b]$.

Remeber: Interpolation $\neq$ Function Approximation.

But we can _use interpolation_ to accurately _approximate_ continuous functions.


```{literalinclude} ../fortran_programs/midterm/main_lagrange.f90
:language: fortran
:linenos: true
```

## Extra Credit Question in Julia

The Fortran program above produced the appximating polynomial over a domain of 50 equally spaced points. Let's use Julia to read these values in, plot the original function $f(x)$, the approximating interpolating polynomial $p(x)$ and the discrete set of data points $(x_i,y_i)$.

```julia
using Plots
using LaTeXStrings
default(linewidth=4, legendfontsize=12)

s = 50
x_domain = LinRange(0,2,s)
p = zeros(length(x_domain))

f(x) = exp.(-3 .* x)

x_data = LinRange(0,2,5)
y_data = f.(x_data)

# open output.txt data and store values in the array called p
open("output.txt","r") do f
    line = 1
    while ! eof(f)
        l = readline(f)
        p[line] = parse.(Float64,l)
        line += 1
    end
end

# Plots
scatter(x_data,y_data, markersize = 6, label = L"(x_i, y_i)")
png(plot!(x_domain, [f.(x_domain) p ], linestyle = [:solid :dash], label = [" exp(-3x)" "p(x)"], title="Lagrange interpolation"), "lagrange_plot.png")
```

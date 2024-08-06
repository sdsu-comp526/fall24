# 4) Intro to C

## Today

 1. Compiling a C program
 2. Variables and Basic Data Types


## 1 Compiling a C program

Contrary to the examples you have seen in the previous lectures in Julia, C programs need to be compiled first, before being executed. This is a drawback of a compiled language (e.g., C, Fortran, Java, etc) Vs an interpreted language (e.g., MATLAB, Python, Julia, etc) in which the interpreter compiles and executes at the same time.

To compile a C program, you need a compiler. This may vary accoriding to the system you use. For:

* Linux: [`gcc`](https://gcc.gnu.org/)
* Mac: [`Xcode/clang`](https://clang.llvm.org/get_started.html)
* Windows: [`Visual Studio C/C++`](https://visualstudio.microsoft.com/vs/features/cplusplus/)

Example in Linux, using `gcc`:

```
gcc hello.c -o hello
```

The `gcc` command invokes the compiler, then you pass the source file you want to compile (`hello.c` in this case), and then after the `-o` target, you can specify the output file for the compiled program (otherwise, the compiler will automatically create `a.out`) for you.

Inspect the directory after compiling, and (if the compilation was successful) you will an object output file (by default `a.out`) or the name of the executable you provided after the `-o` option.

To run/execute your program, simply type `./name_of_your_program` in your terminal:

```
./hello

```

## 2 Variables and Basic Data Types

Our first C program: a `Hello, World!` print statement.

```{literalinclude} ../c_programs/module2-1_intro_to_c/hello.c
:language: c
:linenos: false
```

Displaying Values of Variables:
```{literalinclude} ../c_programs/module2-1_intro_to_c/product.c
:language: c
:linenos: false
```

### Variables
Note:
- In C, you need to declare the variable type before the variable is used
- Variable names begin with letters, `_`, followed by combination of letters, `_`, or digits `0-9`
- Do not use reserved words
- C is **case-sensitive**!

### Global variables
- They have a place in C programming, but are often used to fix badly written code. If multiple parts of your code (like two functions) need to operate on a variable you should use pointers (we'll see more on this later) to share this variable rather than make it available to every function.

### Basic Data Types
- `int`, `float`, `double`, `char` (single character), and `_Bool` (or `bool` if you include the header `stdbool.h`)
- Constant: any number, single character or character string
- Constant expressions: entirely of constant values

**int**
- Integer constant: consist of one or more digits
- Decimal, octal (prefix `0`) and hexadecimal (prefix `0x` or `0X`) notations, with print format specifiers `%o` for octal and `%x` and `%X` for hexadecimal
- Range of values of `int` are associated to the type amount
of storage (size). Typically this is machine and architecture dependent


**float** or **double**
- Floating-point literals (decimal point)
- A variable type double can store roughly twice as many
significant digits as can a variable type float
- Floating and scientific notation (mantissa d. and exponent)
- `%f`, `%e`, `%g` (if `< 4` or `> 5`, `%e` is used)

```c
float length = 190.4;
printf("Length= %f \n",length);
```

**char**
- A variable of type `char` can store a single character (enclosed by quotes)
- Special characters (e.g., `'\n'`)
- `char characterVar = 'z';`
- `printf("Character variable %c\n", characterVar);`

**Bool**
- A variable of type `_Bool` (or `bool`, if you include the header `stdbool.h`) takes only values `0` [or `false`]  and `1` [or `true`]
- When assigning, `0` stores `0` in the variable whereas any nonzero stores `1`
- `_Bool booleanVar = 9;`
- `printf("Boolean variable %i\n", booleanVar);`


**Displaying Values of Variables:**

```{literalinclude} ../c_programs/module2-1_intro_to_c/print_variables.c
:language: c
:linenos: false
```
Output:
```bash
integer variable= 230
float variable = 626.320007
double variable = 3.240000e+03
char variable = a
boolean variable = 0
```

### Const variables
- The `const` qualifier is used for variables whose value will not change in the program

```c
const int base = 10;
```
Any attempt to change its value will generate a compiler error message. Useful to avoid bugs: when you define a variable and use it multiple times in your code, it is also easier this way if you want to change its value, to only change it once where it is defined.

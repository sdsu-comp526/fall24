# 24) Introduction to Fortran

## Last time:
- Community Projects
- Final Project overview

## Today:
 1. How to compile a Fortran program
 2. The simplest Fortran program
 3. Hello, World! and Simple I/O
 4. Arithmetic in Fortran
 5. Functions
  5.1 Subroutines
 6. Interface Blocks
 7. Modules


## 1. How to compile a Fortran program

Historically, Fortran 77, is an older version of the language that still is often used. It is a little simpler in some ways than later versions such as Fortran 90 and Fortran 95. You can see the list of all versions on the [Fortran wiki page](https://en.wikipedia.org/wiki/Fortran).

- Similar to C, Fortran is a _compiled_ language. Hence, before your program can be executed, it must be _compiled_. This converts the English-like text of a Fortran program into a binary form that the processor understands.

You compile a program by typing a line similar to the following:

```bash
gfortran do_nothing.f -o do_nothing
```

It is important that you understand what is happening here:

- `gfortran` here is the name of the Fortran compiler. This is a free compiler that often is included in Linux distributions. Your compiler may have a different name such as `ifort` (from Intel) or `pgf77`.
- `myprogram.f` is the name of the file that contains the source code of your program. The source code is the file of instructions that you actually edit and type. Your source file could be named something other than `myprogram.f`, such as `homework1.f` or some other descriptive name.
- On many systems the name of the source file must end with the suffix `.f` or the compiler will not recognize it as Fortran code.
- The `-o` option tells the compiler that the next word (here, `myprogram`) will be the name of the binary version of the program (or executable for the program). If you omit this option, most systems will use the default name `a.out` for the binary version regardless of the name of your program. It's OK to use this default, though it's usually helpful to use a more meaningful name.
- The binary file is often called the executable file because the computer can run (execute) it.
- On Unix systems (or Linux, or other Unix variants), to run the program, you may need to explicitly state that the program resides in your current directory. The shorthand for the current directory is `.` (a period, or "dot"). Then the program would be run as:

```bash
./myprogram
```

## 2. The simplest Fortran program

The simplest Fortran program is what might be called a "do-nothing" program. It contains all of the required parts of a Fortran program. The program is as follows:

```{literalinclude} ../fortran_programs/module6-1_intro_to_fortran/do_nothing.f
:language: fortran
:linenos: true
```

The program simply starts and stops. It has a beginning (the `PROGRAM` instruction), and then tells the system to halt execution (`STOP`). Notice that there is both `STOP` and `END`. The difference is that `STOP` tells the system to halt the execution of the program, while `END` identifies the end of your instructions - that is, `END` will always appear as the last line of a program.

Complex programs may have more than one `STOP` instruction; for example, execution may be halted if an error occurs or a limit is reached. A single instruction in Fortran is usually called a _statement_.

Although seemingly trivial, writing and executing this program illustrates some of the basic rules of writing Fortran programs:

- Only one instruction (or statement) may appear on a line.
- The statement is limited to the first 72 characters of each line. Many modern Fortran 77 compilers will accept longer lines, but this is not standard.
- The program is written in all uppercase letters. Strictly speaking, uppercase is the standard for Fortran 77 but almost all Fortran compilers will accept lowercase letters. (Such compilers are not "case sensitive".) We will use uppercase because it helps to set off the program from the surrounding explanation. You can use either upper or lower case, whichever you like.
- Notice that all of the lines in the example above begin with 6 blank spaces. This is because the Fortran 77 standard requires that the first 6 characters of a line be reserved for statement numbers and continuation characters. These 6 spaces originate from the punched card version of Fortran.

### Exercise 24.1:

Create your first "do nothing" Fortran program.

## 3. Hello, World! and Simple I/O

The next simplest Fortran program is something programmers call the "Hello World" program. This is the simplest program that actually produces some output. An example of the "Hello World!" program in Fortran is:

```{literalinclude} ../fortran_programs/module6-1_intro_to_fortran/hello_world.f
:language: fortran
:linenos: true
```

- Notice that we used the `WRITE` statement rather than the `PRINT` statement.
- The `WRITE` statement writes output to the screen when the program is run interactively. In fact it writes to something called "standard output," which defaults to the screen.
- The asterisk in the `PRINT` or `WRITE` statment specifies "free format" output. This essentially means that we aren't interested in the exact format in which the system prints the result.
- The text you want to print out is inside single quotes.

### Exercise 24.2:

 1. Type the "Hello World" program yourself. Compile it and run it.
 2. Change the "Hello World" program to print out something else, such as your name.

### Simple I/O

To print out on a file use the following statements in your program:

```fortran
WRITE(*,*) 'Hello, world!' ! this prints out on the screen
OPEN(1,file='MyFile.dat') ! this assigns an ID number to the file
WRITE(1,99)'Hello world!' ! this prints out to the file with the format specifier 99
CLOSE(1)
CALL flush(1)
99 FORMAT(A25) ! format specifier for your data. You need the file extension .f90 for this
```

To read from a file:

```fortran
CHARACTER(len = 25) :: x
OPEN(1,file='MyFile.dat')
READ(1,99) x ! we read the data from file and put it into x
OPEN(2,file='MyFile2.dat',STATUS='REPLACE',ACTION='WRITE') ! this assigns an ID number to the file MyFile2
WRITE(2,99)'Hello world, again!' ! this prints out to the file with the format specifier 99
CALL flush(1)
CALL flush(2)
CLOSE(1)
CLOSE(2)

99 FORMAT(A25)
```

## 4. Arithmetic in Fortran: Real and Integer Variables

Fortran has several different types of variables. The two most commonly used types for computations are _integer_ variables and _real_ variables.

- _Integer variables_ do not have a fractional part. The value of an integer variable is a whole number, like `0`, `2`, `5`, `-76`, and so on.

- _Real variables_ have a fractional part, which may be zero. These variables have values like `2.0` or `3.1416`. Real variables are sometimes called floating-point variables.

Important! For an integer variable, the fractional part is not merely zero. The fractional part does not exist:

- `2` is an integer value
- `2.0` is a real value

The type of each variable should be declared at the beginning of the program, right after the program is named.
- The type declarations have to appear before the first executable statement.

Additionally, your variable names should adhere to the following conventions:

- The variable name may include both letters and digits, but the first character of the variable name has to be a letter. For example, `A2` is an allowable name, but `2A` is not.
- In standard Fortran 77 each variable name has to be six characters or less, but almost all modern systems will accept longer variable names.

Following is a program that illustrates some of the differences between real and integer variables:

```{literalinclude} ../fortran_programs/module6-1_intro_to_fortran/variables.f
:language: fortran
:linenos: true
```

- Any text that you want to print out is inside single quotes, but when printing the value of a variable you don't put it in quotes.
- If you want to print multiple things (whether text or values) separate them with a comma.

### Exercise 24.3:

 1. Type the program as listed above, compile it and run it.
 2. What happens to the value of _pi_ when it gets converted to an integer?
 3. What happens to the value of _e_ when it gets converted to an integer? Look closely: Is the result rounded to the nearest integer?
 4. Change the values of _pi_ and _e_ to negative numbers (that is, -3.1416 and -2.71828), then re-compile and re-run the program. What happens when the negative values are converted to integers?

### Other variable types

Examples:

```fortran
      CHARACTER(len=*), PARAMETER :: Name = 'YourName' ! defines a constant string of characters of unknown length
      INTEGER, PARAMETER :: n = 10
      REAL, PARAMETER :: Dx = 1.0d0
      REAL :: x(n) ! declares an array of n real values called x
```

- Note that in the example above we have used the `PARAMETER` keyword, which is used to assign a symbolic name to a constant. In many places, one just wants to assign a name to a particular value. For example, keep typing 3.1415926 is tedious. In this case, one could assign a name, say `PI`, to 3.1415926 so that one could use `PI` rather than 3.1415926.
- To declare a `PARAMETER`, add `PARAMETER` in front of the double colon (`::`) and use a comma to separate the type name (i.e., `REAL`) and the word `PARAMETER`.
- Following each name, one should add an equal sign (`=`) followed by an expression. The value of this expression is then assigned the indicated name.
- After assigning a name to a value, one can use the name, rather than its value throughout the program. The compiler would convert that name to its corresponding value.
- It is important to note that the name assigned to a value is simply an _alias_ of the value. Therefore, that name is not a variable.
- After assigning a name to a value, that name can be used in a program, even in subsequent type statements.

#### Restrictions

- A symbolic constant must not be defined more than once in a program unit.
- If a symbolic name appears in a `PARAMETER` statement, then it cannot represent anything else in that program unit.
- A symbolic name cannot be used in a constant format specification, but it can be used in a variable format specification.
- If you pass a parameter as an argument, and the subprogram tries to change it, you may get a runtime error.

## 5. Functions

- Similar to other languages, in Fortran a function is a self-contained unit that receives some input from the outside via its arguments, performs a task, and then returns the result.
- A Fortran function is a procedure whose result is a single number, logical value, character string or array.
- This result can be be used to form a Fortran expression.
  * The expression may be on the right side of an assignment statement.
- There are two types of functions, intrinsic and user-defined.
  * Intrinsic functions are those functions built into a Fortran language, such as `SIN(x)` or `LOG(x)`.

User-defined functions are functions defined by programmers to meet a specific need not addressed by the standard intrinsic functions.

General form of user-defined functions:

```fortran
      FUNCTION function_name(argument list)
	  !! DECLARACTIONS

	  !! EXECUTABLES

	  function_name = expression

      RETURN	! ONLY NEEDED IF WE PLAN TO REACH END FUNCTION ALL OF THE TIME
      END FUNCTION function_name
```

- A function is invoked (or called) by naming it in an expression.
- Function names follow the same rules as variable names.
- The name of the function _must_ appear on the left side of at least one assignment statement in the function:

```fortran
      function_name = expression
```

- The argument list of the function may be blank if the function can perform all calculations with no input arguments.
- The parentheses around the argument list are required even if the list is blank.
- Since the function returns a value, it is necessary to assign a type to that function.
- The type of the function must be declared both in the function procedure and the calling programs.
- In Fortran, we need to specify the types of function arguments.
  * All arguments must be declared with a new attribute
```fortran
      INTENT(IN)
```

- The meaning of `INTENT(IN)` indicates that the function will only use this argument as input and must not change its value.

Example:

```{literalinclude} ../fortran_programs/module6-1_intro_to_fortran/larger_root_function.f
:language: fortran
:linenos: true
```

:::{note}
Remember that to be able to call this function, we need at least a `main` `PROGRAM`.
:::

- Function subprograms and any other subprograms are placed after the `END` statement of the `main` program (unless they are internal functions - then, they need to appear in `CONTAINS` section).

### Common mistakes

1) Forgetting the function type (i.e., the type of the return argument).

Example:
```fortran
      FUNCTION DoSomething(a,b)
        IMPLICIT NONE

        INTEGER, INTENT (IN) :: a, b

        DoSomething = SQRT(a*a+b*b)
      END FUNCTION DoSomething
```
If there is no type, you will not be able to determine the returned value type.

2) Forgetting `INTENT(IN)` for input variables.

Example:

```fortran
      REAL FUNCTION DoSomething(a,b)

        IMPLICIT NONE
        INTEGER :: a,b

        DoSomething = SQRT(a*a + b*b)
      END FUNCTION DoSomething
```
Actually, this is _not_ an error. But, without `INTENT (IN)`, the compiler will not be able to check many potential errors.

3) Changing value of formal argument declared with `INTENT(IN)`.

Example:

```fortran
      REAL FUNCTION DoSomething(a,b)

        IMPLICIT NONE
        INTEGER, INTENT (IN) :: a,b

        IF(a>b) THEN
            a = a - b
        ELSE
            a = a + b
        END IF

        DoSomething = SQRT(a*a+b*b)
      END FUNCTION DoSomething
```

Since `a` was declared with `INTENT(IN)`, its value cannot be changed.

4) Forgetting to store value to function name.

Example:

```fortran
      REAL FUNCTION DoSomething(a,b)

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: a,b
        INTEGER :: c
        c = SQRT( a ∗ a + b ∗ b )
      END FUNCTION DoSomething
```

Since there is no value ever stored in `DoSomething`, the returned value could be anything (garbage).

5) Function name used in right hand side of expression.

Example:

```fortran
      REAL FUNCTION DoSomething (a,b)

        IMPLICIT NONE
        INTEGER, INTENT(IN) :: a,b

        DoSomething = a*a+b*b
        DoSomething = SQRT(DoSomething)
      END FUNCTION DoSomething
```

Only a special type of functions, _recursive_ functions, could have their names on the right-hand side of expressions (we don't cover recursion in this lecture, because as we've seen before it is pretty impractical).

6) The most recent value stored in function name is returned.

Example:

```fortran
      REAL FUNCTION DoSomething(a,b)

        IMPLICIT NONE
        INTEGER, INTENT(IN) :: a,b
        DoSomething = a*a+b*b
        DoSomething = SQRT(a*a-b*b)
      END FUNCTION DoSomething
```

The second assignment overwrites the previous value.

Some important rules for Fortran function arguments:

- If an actual argument is an expression, it is evaluated and the result is saved into a temporary location. Then, the value in this temporary location is passed.
- If an actual argument is a constant, it is considered as an expression. Therefore, its value is saved to a temporary location and then passed.
- If an actual argument is a variable, its value is taken and passed to the corresponding formal argument.
- If an actual argument is a variable enclosed in a pair of parenthesis like `(A)`, then this is an expression and its value is evaluated and saved to a temporary location. Then, this value (in the temporary location) is passed.
- For a formal argument declared with `INTENT(IN)`, any attempt to change its value in the function will cause a compiler error.
- In a Fortran function, the `INTENT` should always be `IN`. If you plan to use an `INTENT` other than `IN` then consider using a `SUBROUTINE` rather than a `FUNCTION` (see below).


### 5.1 Subroutines

- We saw that Fortran functions have an explicit return type and are intended to return only one value.
- _Subroutine_ subprograms, on the other hand, have no explicit type and return multiple or no values through a parameter call list.
- Unlike functions, calls to subroutines cannot be placed in an expression.
- In the main program, a subroutine is activated by using a `CALL` statement which include the subroutine name followed by the list of inputs and outputs surrounded by parenthesis. The inputs and outputs are collectively called the _arguments_
- A subroutine name follows the same rules as for function names and variable names: historically, less than six letters and numbers, and must begin with a letter. Because of this, subroutine names should be different than those used for variables or functions.
- As with functions, there are some rules for using subroutines. Keep these in mind when writing your subroutines:
  * You do not need to declare the subroutine name in the main program as you do with a function name.
  * They begin with a line that includes the word `SUBROUTINE`, the name of the subroutine, and the arguments for the subroutine.
- The `INTENT` of arguments in subroutines can be multiple: `IN` (the value of the dummy argument may be used, but not modified, within the procedure.), `OUT` (the dummy argument may be set and then modified within the procedure, and the values returned to the caller), and `INOUT` (initial values of the dummy argument may be both used and modified within the procedure, and then returned to the caller).

## 6. Interface blocks

- Safety feature which allows main programs and external subprograms to interface appropriately with your user-defined function/subroutine.

- Ensures that the calling program and the subprogram have the correct number and type of arguments.

- Helps compiler to detect incorrect usage of a subprogram at compile time.

- It consists of:
  1. Number of arguments
  2. Type of each argument
  3. Type of values returned by the subprogram

Example:
```{literalinclude} ../fortran_programs/module6-1_intro_to_fortran/area_circle.f90
:language: fortran
:linenos: true
```

## 7. Modules

- It is often the case that there are parameters, variables, and subprograms that must be shared by several program units.

- Fortran 90 provides a special program unit known as a `MODULE` that conveniently packages collections of declarations and subprograms so that they may be imported into other program units.

Syntax:
```fortran
      MODULE module_name
      ! some specifications
      END MODULE module_name
```

- A program module is made accessible to the various program units by way of the `USE` statement.

- The `USE` statement must appear at the beginning of the declaration part of the program unit making use of the module. It must appear even _before_ the statement `IMPLICIT NONE`.

Example:

```{literalinclude} ../fortran_programs/module6-1_intro_to_fortran/using_modules.f
:language: fortran
:linenos: true
```

- Each module unit must be compiled independently.

- To compile a module use:

```bash
gfortran -c math_consts_module.f
```
which will produce `math_consts_module.o` and `math_consts_module.mod` files

- To compile the main program that uses the module, use:

```bash
gfortran -Wall using_modules.f math_consts_module.o -o using_modules
```
This will both compile your main program `using_modules.f` _and_ link your compiled module.

Alternatively, you could have compiled the module and main program together, as in:

```bash
gfortran -ffree-form -c math_consts_module.f using_modules.f
```

And then you could have done the linking of the object files:

```bash
gfortran math_consts_module.o using_modules.o -o using_modules
```

Review of Fortran modules:

- Modules provide you a way of splitting your programs between multiple files.
- Modules are used for:
  * Packaging subprograms, data and interface blocks
  * Defining global data that can be used by more than one routine
  * Declaring variables that can be made available within any routines you choose
  * Importing a module entirely, for use, into another program or subroutine.
- Syntax of a module (two parts):
  * a specification part for statements declaration
  * a contains part for subroutine and function definitions

General form:

```fortran
      MODULE module_name
      ! [ statement declarations ]
      ! [ contains subroutine and function definitions]
      END MODULE module_name
```

Using a module:

```fortran
      USE module_name
```

Note:

- Can add as many modules as needed, each will be in separate files and compiled separately.
- A module can be used in various different programs.
- A module can be used many times in the same program.
- The variables declared in a module specification part, are global to the module
- The variables declared in a module become global variables in any program or routine where the module is used
- The use statement can appear in the main program, or any other subroutine or module which uses the routines or variables declared in a particular module



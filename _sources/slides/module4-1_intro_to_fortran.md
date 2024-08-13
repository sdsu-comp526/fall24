# 14) Introduction to Fortran

Today:
 1. How to compile a Fortran program
 2. The simplest Fortran program
 3. Hello, World! and Simple I/O
 4. Arithmetic in Fortran



## 1. How to compile a Fortran program

Historically, Fortran 77, is an older version of the language that still is often used. It is a little simpler in some ways than later versions such as Fortran 90 and Fortran 95.

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

```{literalinclude} ../fortran_programs/module4-1_intro_to_fortran/do_nothing.f
:language: fortran
:linenos: true
```

The program simply starts and stops. It has a beginning (the `PROGRAM` instruction), and then tells the system to halt execution (`STOP`). Notice that there is both `STOP` and `END`. The difference is that `STOP` tells the system to halt the execution of the program, while `END` identifies the end of your instructions - that is, `END` will always appear as the last line of a program.

Complex programs may have more than one `STOP` instruction; for example, execution may be halted if an error occurs or a limit is reached. A single instruction in Fortran is usually called a _statement_.

Although seemingly trivial, writing and executing this program illustrates some of the basic rules of writing Fortran programs:

- Only one instruction (or statement) may appear on a line.
- The statement is limited to the first 72 characters of each line. Many modern Fortran 77 compilers will accept longer lines, but this is not standard.
- The program is written in all uppercase letters. Strictly speaking, uppercase is the standard for Fortran 77 but almost all Fortran compilers will accept lowercase letters. (Such compilers are not "case sensitive".) We will use uppercase because it helps to set off the program from the surrounding explanation. You can use either upper or lower case, whichever you like.
- Notice that all of the lines in the example above begin with 6 blank spaces. This is because the Fortran 77 standard requires that the first 6 characters of a line be reserved for statement numbers and continuation characters.

### Exercise 14.1:
Create your first "do nothing" Fortran program.

## 3. Hello, World! and Simple I/O
The next simplest Fortran program is something programmers call the "Hello World" program. This is the simplest program that actually produces some output. An example of the "Hello World!" program in Fortran is:

```{literalinclude} ../fortran_programs/module4-1_intro_to_fortran/hello_world.f
:language: fortran
:linenos: true
```

- Notice that we used the `WRITE` statement rather than the `PRINT` statement from the one in the previous exercise.
- The `WRITE` statement writes output to the screen when the program is run interactively. In fact it writes to something called "standard output," which defaults to the screen.
- The asterisk in the `PRINT` or `WRITE` statment specifies "free format" output. This essentially means that we aren't interested in the exact format in which the system prints the result.
- The text you want to print out is inside single quotes.

### Exercise 14.2:
 1. Type the "Hello World" program yourself. Compile it and run it.
 2. Change the "Hello World" program to print out something else, such as your name.

### Simple I/O
To print out on a file use the following statements in your program:

```fortran
OPEN(1,file='MyFile.dat') ! this assigns an ID number to the file
WRITE(1,99)Hello world! ! this prints out to the file with the format specifier 99
CLOSE(1)
CALL flush(1)
99 FORMAT(ES15.5) ! format specifier for your data: one column of 15 digits width, with exponential format and 5 decimals.
```

To read from a file:

```fortran
OPEN(2,file='MyFile2.dat')
READ(2,99) x ! we read the data from file 2 and put it into x
CALL flush(1)
CLOSE(2)
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

```{literalinclude} ../fortran_programs/module4-1_intro_to_fortran/variables.f
:language: fortran
:linenos: true
```

- Any text that you want to print out is inside single quotes, but when printing the value of a variable you don't put it in quotes.
- If you want to print multiple things (whether text or values) separate them with a comma.

### Exercise 14.3:

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












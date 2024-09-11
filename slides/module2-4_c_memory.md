# 7) C memory

Today:
 1. Pass by value Vs Pass by reference
 2. C pools of memory
 3. `malloc` and `calloc`

## 1. Pass by value Vs Pass by reference

Functions can be invoked in two ways: **Call by Value** or **Call by Reference**. These two ways are generally differentiated by the type of values _passed_ to them as parameters.

The parameters passed to the function are called **actual parameters** whereas the parameters received by the function are called **formal parameters**.

### 1.1 Call by Value in C

In the call by value method of parameter passing, the values of actual parameters are copied to the function’s formal parameters.

- There are two copies of parameters stored in different memory locations.
- One is the original copy and the other is the function copy.
- Any changes made inside functions are _not_ reflected in the actual parameters of the caller.

#### Example of Call by Value in C

The following example demonstrates the call-by-value method of parameter passing

```{literalinclude} ../c_programs/module2-2_c_pointers/pass_by_value.c
:language: c
:linenos: true
```

Output:
```bash
Inside the (callee) function:
 x = 20 y = 10
In the caller:
 a = 10 b = 20
```

Thus the actual values of `a` and `b` remain unchanged even after swapping the values of `x` and `y` in the called function.

### 1.2 Call by Reference in C

In the call by reference method of parameter passing, the _address_ of the actual parameters is passed to the function as the formal parameters. In C, we use pointers to achieve call-by-reference.

- Both the actual and formal parameters refer to the same locations.
- Any changes made inside the function are reflected in the actual parameters of the caller function, too.

#### Example of Call by Reference in C

The following C program is an example of a call-by-reference method.

```{literalinclude} ../c_programs/module2-2_c_pointers/pass_by_reference.c
:language: c
:linenos: true
```

Output:

```bash
Inside the (callee) function:
 x = 20 y = 10
In the caller:
 a = 20 b = 10
```
Thus, the values of `a` and `b` get changed also in the caller function after swapping the values of `x` and `y` inside the callee.

#### Difference between the Call by Value and Call by Reference in C

The following table lists the differences between the call-by-value and call-by-reference methods of parameter passing.

|Call/Pass By Value     | Call/Pass By Reference
|:--------------------:|:-----------------------------:|
|  While calling a function, we pass directly the values of variables to it. Such functions are known as "Call By Value"  | While calling a function, instead of passing the actual values of the variables, we pass the _address of_ the variables (location of variables) to the function. This is known as "Call By Reference" |
| In this method, the value of each variable in the calling function is _copied_ into corresponding dummy variables in the called function | In this method, the _address of_ the actual variables in the calling function is copied into the dummy variables in the called function. |
| With this method, changes made to the dummy variables in the called function have _no effect_ on the values of the actual variables in the calling function  | With this method, using addresses (and pointers associated with them), we have access to the actual variables and hence we can _manipulate_ them  |
| In call-by-value, we cannot alter the values of the original variables through function calls   | In call by reference, we can alter the values of variables through function calls   |
| Values of variables are directly passed to functions  | Pointer variables are necessary in the function declaration/definition to access the addresses of the passed variables  |
| This method is preferred when we have to pass some small values that should not change  | This method is preferred when we have to pass a large amount of data to the function or when data need to be manipulated  |

In conclusion, "Call/Pass by Value" means passing values as copies to the function, so that the original data is preserved and any changes made inside the function are _not_ reflected in the original data, whereas "Call/Pass by Reference" means passing references to the memory locations of variables (in C, we use pointers to achieve the call by reference behavior), hence changes made inside the called function are reflected in the original caller values.


## 2. C pools of memory

C has three different pools of memory.

- **static**: global variable storage, permanent for the entire run of the program.
- **stack**: local variable storage (automatic, continuous memory).
- **heap**: dynamic storage (large pool of memory, not allocated in contiguous order. Can be fragmented).

![](../img/c_stack_memory.jpg)


### Static memory
- Static memory persists throughout the entire life of the program, and is usually used to store things like _global_ variables, or variables created with the static clause. For example, we saw the simple declaration of an `int` variable:

```c
int int_var;
```

On many systems this variable uses 4 bytes of memory. This memory can come from one of two places. If a variable is declared _outside_ of a function, it is considered global, meaning it is accessible anywhere in the program. Global variables are **static**, and there is only one copy for the entire program. Inside a function the variable is allocated on the _stack_.
- It is also possible to force a variable to be static using the `static` clause. For example, the same variable created inside a function using the `static` clause would allow it to be stored in static memory.

```c
static int int_var;
```

### Stack memory

The stack is used to store variables used on the inside of a function (including the `main()` function). It’s a **LIFO**, "**L**ast-**I**n,-**F**irst-**O**ut”, structure. Every time a function declares a new variable it is "pushed" onto the stack. Then when a function finishes running, all the variables associated with that function on the stack are automatically deleted, and the memory they use is freed up. This leads to the _local_ scope of function variables. The stack is a special region of memory, and automatically managed by the CPU – so you don’t have to allocate or deallocate memory. Stack memory is divided into successive frames where each time a function is called, it allocates itself a fresh stack frame.

- Note that there is generally a limit on the size of the stack – which can vary with the operating system (it can be, say, 8MB). If a program tries to put too much information on the stack, **stack overflow** will occur. Stack overflow happens when all the memory in the stack has been allocated, and further allocations begin overflowing into other sections of memory.

A summary of the stack:

- The stack is managed by the CPU, there is no ability to modify it
- Variables are allocated and freed automatically
- The stack it not limitless – most have an upper bound
- The stack grows and shrinks as variables are created and destroyed, within the limit imposed by the OS
- Stack variables only exist whilst the function that created them exists

### Heap memory

The _heap_ is a large pool of memory that can be used dynamically, i.e, especially when you don't know at compile time how big a variable is (for instance, think of an array that has user-defined length). This is memory that is _not_ automatically managed – you have to explicitly allocate (using functions such as `malloc`), and deallocate (e.g. `free`) the memory. Failure to free the memory when you are finished with it will result in what is known as a **memory leak** – memory that is still "being used", and not available to other processes. Unlike the stack, there are generally no restrictions on the size of the heap (or the variables it creates), other than the physical size of memory in the machine. Variables created on the heap are accessible anywhere in the program.

- Heap memory requires you to use pointers.

A summary of the heap:

- The heap is managed by the programmer, the ability to modify it is somewhat boundless
- In C, variables are allocated and freed using functions like `malloc()` and `free()`
- The heap is large, and is usually limited by the physical memory available
- The heap requires pointers to access it

An example of memory use:

```{literalinclude} ../c_programs/module2-4_c_memory/c_memory_pools.c
:language: c
:linenos: true
```

Output:
```bash
stack memory: 4
heap memory: m
```

The variable `x` is _static_ storage, because of its global nature. Both `y` and `str` are dynamic _stack_ storage which is deallocated when the program ends. The function `malloc()` is used to allocate 100 pieces of of dynamic _heap_ storage, each the size of char, to `str`. Conversely, the function `free()`, deallocates the memory associated with `str`.

![](../img/c_stack_memory_example.jpg)

### Reading exercise
For the advantages and disadvantages of using one type of memory rather than another, please see this [resource](https://www.geeksforgeeks.org/stack-vs-heap-memory-allocation/).

## 3. `malloc` and `calloc`

In the previous example, we have seen an example of usage of the function `malloc()`.

- `malloc()` allocates a memory block of given size (in bytes).
- `malloc()` returns a pointer to the beginning of the allocated block.

:::{note}
`malloc()` doesn’t initialize the allocated memory. If you try to read from the allocated memory without first initializing it, then you will invoke undefined behavior, which usually means the values you read will be garbage values.
:::

- `calloc()` allocates the memory and also initializes every byte in the allocated memory to 0.
- If you try to read the value of the allocated memory without initializing it, you’ll get 0 as it has already been initialized to 0 by `calloc()`.

Unlike `malloc()`, `calloc()` takes two arguments:

- Number of blocks to be allocated.
- Size of each block in bytes.

**Return Value:**

After successful allocation in `malloc()` and `calloc()`, a pointer to the block of memory is returned otherwise `NULL` is returned which indicates failure.

### Example of a program that shows both `malloc` and `calloc`


```{literalinclude} ../c_programs/module2-4_c_memory/malloc_and_calloc.c
:language: c
:linenos: true
```

Output:
```bash
Values of allocated_with_calloc: 0 0 0 0 0
The allocation failed, the value of failed_malloc is: (nil)
```

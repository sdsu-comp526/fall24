# 5) Intro to C pointers

The material for this class is largely adapted from this great [SDSU resource](https://edoras.sdsu.edu/doc/c/pointers-1.2.2/).

## Definition
A pointer is a memory address.

## Examples
Say you declare a variable named foo.

```c
int foo;
```

This variable occupies some memory. On a PowerPC, it occupies four bytes of memory (because an int is four bytes wide).

Now let’s declare another variable.

```c
int *foo_ptr = &foo;
```

`foo_ptr` is declared as a pointer to a type `int`. We have initialized it to point to `foo`.

`foo` occupies some memory. Its location in memory is called its address. `&foo` is the address of `foo` (which is why `&` is called the "address-of operator").

Think of every variable as a box. `foo` is a box that is `sizeof(int)` bytes in size. The location of this box is its address. When you access the address, you actually access the contents of the box it points to.

This is true of all variables, regardless of type. In fact, grammatically speaking, there is no such thing as a "pointer variable": all variables are the same. There are, however, variables with different types. `foo`’s type is int. `foo_ptr`’s type is `int *`. (Thus, "pointer variable" really means "variable of a pointer type".)

![](../img/pointers_boxes.png)

The sense of that is that the pointer is not the variable. The pointer to `foo` is the contents of `foo_ptr`. You could put a different pointer in the `foo_ptr` box, and the box would still be `foo_ptr`. But it would no longer point to `foo`.

The pointer has a type, too, by the way. In this case, its type is `int`. Thus it is an "int pointer" (a pointer to `int`). An int `**`’s type is `int *` (it points to a pointer to int). The use of pointers to pointers is called multiple indirection. More on that in a bit.

### Print some addresses
Let's visualize this.

- Every variable is associated to a memory location and every
memory location has an address.

- Addresses in memory can be accessed using ampersand (`&` or the address operator) and the printf `%p` formatting option.

```c
#include <stdio.h>

int main (void)
{

    double var1;
    int var2;
    char var3 [5];

    printf("Address of var1 is %p \n", &var1);
    printf("Address of var2 is %p \n", &var2);
    printf("Address of var3 is %p \n", &var3);
    return 0;

}
```

Output:
```bash
Address of var1 is 0x7fff804e0028
Address of var2 is 0x7fff804e0024
Address of var3 is 0x7fff804e0033
```

## Declaration syntax
To declare multiple variables (of the same type) in one statement, you can just simply separate their names by commas, after the type declaration, i.e.,

```c
int* ptr_a, ptr_b; // two pointers of the type int
```

**Note:** this is true for all types of variables, not only pointers.

:::{tip}
The space matters!
You would think that the type of `ptr_b` would be `int *`, as it is `ptr_a`, right? Wrong!!
:::

The type of `ptr_b` is only `int`. It is **not** a pointer to an `int`.

C’s declaration syntax ignores the pointer asterisks when carrying a type over to multiple declarations. If you split the declaration of `ptr_a` and `ptr_b` into multiple statements, you get this:

```c
int *ptr_a;
int  ptr_b;
```

It’s possible to do the single-line declaration in a clear way. This is the immediate improvement:

```c
int *ptr_a, ptr_b;
```

:::{tip}
Note: the asterisk has moved. It is now right next to the word `ptr_a`.
:::

If you find the above not easy to read and bug-prone (and I might agree with you), you could equally specify each variable with its own type, to avoid confusion:

```c
int *ptr_a, *ptr_b;

```

If, instead, you want to declare one type `int` and one type pointer to int, the convention is to put the non-pointer variable first, as in

```c
int ptr_b, *ptr_a;

```

Incidentally, C allows zero or more levels of parentheses around the variable name and asterisk:

```c
int ((not_a_pointer)), (*ptr_a), (((*ptr_b)));
```

## Assignment and pointers
Now, how do you assign an `int` to this pointer? This solution might be obvious:

```c
foo_ptr = 42;
```

It is also wrong.

Any direct assignment to a pointer variable will change the address in the variable, _not_ the value at that address. In this example, the new value of `foo_ptr` (that is, the new "pointer" in that variable) is 42. But we don’t know that this points to anything, so it probably doesn’t. Trying to access this address will probably result in a segmentation violation (i.e., crash).

(Incidentally, compilers usually warn when you try to assign an int to a pointer variable. `gcc` will say `warning: initialization makes pointer from integer without a cast`.)

So how do you access the value at a pointer? You must dereference it.

## Dereferencing

```c
int bar = *foo_ptr;
```

In this statement, the dereference operator (prefix `*`, not to be confused with the multiplication operator) looks up the value that exists at an address. (On the PowerPC, this called a `load` operation.)

It’s also possible to write to a dereference expression (the C way of saying this: a dereference expression is an `lvalue`, meaning that it can appear on the left side of an assignment):

```c
*foo_ptr = 42; // Sets foo to 42
```

(On the PowerPC, this is called a `store` operation.)

## Other type examples

```c
int ∗ip;     // pointer to an int
double ∗dp;  // pointer to a double
float ∗fp;   // pointer to a float
char ∗ch;    // pointer to a char
```

## Arrays


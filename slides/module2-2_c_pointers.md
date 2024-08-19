# 5) Intro to C pointers

Today:
 1. Definition
 2. Declaration Syntax
 3. Assignment and pointers
 4. Dereferencing
 5. Pointer arithmetic
 6. Indexing
 7. Indirection or pointer to pointer


The material for this class is largely adapted from this great [SDSU resource](https://edoras.sdsu.edu/doc/c/pointers-1.2.2/).

## 1 Definition
A pointer is a memory address.

## Examples
Say you declare a variable named foo.

```c
int foo;
```

This variable occupies some memory. On modern architectures, it occupies four bytes of memory (because an int is four bytes wide).

Now let’s declare another variable.

```c
int *foo_ptr = &foo;
```

`foo_ptr` is declared as a pointer to a type `int`. We have initialized it to point to `foo`.

`foo` occupies some memory. Its location in memory is called its address. `&foo` is the address of `foo` (which is why `&` is called the "address-of operator").

Think of every variable as a box. `foo` is a box that is `sizeof(int)` bytes in size. The location of this box is its address. When you access the address, you actually access the contents of the box it points to.

This is true of all variables, regardless of type. In fact, grammatically speaking, there is no such thing as a "pointer variable": all variables are the same. There are, however, variables with different types. `foo`’s type is int. `foo_ptr`’s type is `int *`. (Thus, "pointer variable" really means "variable of a pointer type".)

![](../img/pointers_boxes.png)

The sense of that is that the pointer is not the variable. The pointer to `foo` is the contents of `foo_ptr`. You could refer to a different address in the `foo_ptr` box, and the box would still be `foo_ptr`. But it would no longer point to `foo`.

The pointer has a type, too, by the way. In this case, its type is `int`. Thus it is an "int pointer" (a pointer to `int`). An `int **`’s type is `int *` (it points to a pointer to int). The use of pointers to pointers is called multiple indirection. More on that in a bit.

### Print some addresses
Let's visualize this.

- Every variable is associated to a memory location and every
memory location has an address.

- Addresses in memory can be accessed using ampersand (`&` or the address operator) and the printf `%p` formatting option.

```{literalinclude} ../c_programs/module2-2_c_pointers/print_addresses.c
:language: c
:linenos: false
```

Output:
```bash
Address of var1 is 0x7fff804e0028
Address of var2 is 0x7fff804e0024
Address of var3 is 0x7fff804e0033
```

## 2 Declaration syntax
To declare multiple variables (of the same type) in one statement, you can just simply separate their names by commas, after the type declaration, i.e.,

```c
int* ptr_a, ptr_b; // two pointers of the type int
```

**note:** this is true for all types of variables, not only pointers.

:::{note}
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

:::{note}
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

## 3 Assignment and pointers
Now, how do you assign an `int` to this pointer? This solution might be obvious:

```c
foo_ptr = 42;
```

It is also wrong.

Any direct assignment to a pointer variable will change the address in the variable, _not_ the value at that address. In this example, the new value of `foo_ptr` (that is, the new "pointer" in that variable) is 42. But we don’t know that this points to anything, so it probably doesn’t. Trying to access this address will probably result in a segmentation violation (i.e., crash).

(Incidentally, compilers usually warn when you try to assign an int to a pointer variable. `gcc` will say `warning: initialization makes pointer from integer without a cast`.)

So how do you access the value at a pointer? You must dereference it.

## 4 Dereferencing

```c
int bar = *foo_ptr;
```

In this statement, the dereference operator (prefix `*`, not to be confused with the multiplication operator) looks up the value that exists at an address.

It’s also possible to write to a dereference expression:

```c
*foo_ptr = 42; // Sets foo to 42
```

### Other type examples

```c
int *ip;     // pointer to an int
double *dp;  // pointer to a double
float *fp;   // pointer to a float
char *ch;    // pointer to a char
```

### A word on Arrays and Pointers

We have not introduced Arrays yet, but let's just add an interlude for the purpose of illustrating their relationship with pointers.

If you declare a three-`int` array (i.e., an array that contains three `int`s) with the following statement

```c
int array[] = {42, 77, 89};
```
note that we are using the `[]` notation to indicate that this is a collection of items (an array). If we had accidentally declared this as a pointer to a collection of items, i.e., `int *array[] = {42, 77, 89}`, the compiler would have errored, not accepting the assigment of the elements `{42, 77, 89}` to initialize the array variable.

This variable, `array`, is an extra-big box: three `int`s’ worth of storage.

If you try to display the content of the array by printing via

```c
printf("Array %p\n", array); // it will print some hexadecimal string like 0x7fffa66c0fac
```
Note that we used the format identifier `%p` for pointers. But when we use the _name_ of the array, `array`, you are actually using a pointer to the first element of the array (in C terms, `&array[0]`). This is called 'decaying': the array 'decays' to a pointer. Any usage of `array` is equivalent to if `array` had been declared as a pointer (with some caveats: you can’t assign to it or increment or decrement it, like you can with a real pointer variable - we will see more of this later).

So when you passed `array` to `printf`, you really passed a pointer to its first element.

'Decaying' is like an implicit 'address-of' `&` operator.

:::{note}
```c
array == &array == &array[0]
```
:::

In English, these expressions read "`array`", "address of `array`", and "address of the first element of `array`" (in the latter, the index operator, `[]`, has higher precedence than the address-of operator). But in C, all three expressions mean the same thing.

So, how do we actually print the _content_ of `array`? Before printing a multi-entry variable like a whole array (we'll see this just in a bit), let's recap how it looks like for a single element:

```c
# include <stdio.h>

int main(void) {

    int var = 20; // an int variable declaration
    int *iptr;    // pointer variable declaration
    iptr = &var;  // store address of var in pointer variable
    printf("Address of var variable: %p \n", &var); //address stored in pointer variable
    printf("Address stored in iptr variable: %p\n", iptr); // access the value using the pointer
    printf("Value of ∗iptr variable: %d \n", *iptr); // what iptr points to
    printf("Value of int variable: %d \n", var); // just the plain int variable

    return 0;
}
```

It produces the following output

```bash
Address of var variable: 0x7ffe645cc05c
Address stored in iptr variable: 0x7ffe645cc05c
Value of ∗iptr variable: 20
Value of int variable: 20
```

### Exercise 5.1
Check out and run the `pointer-dereferencing.c` program.

### A word on NULL pointers

- It is always a good practice to assign a `NULL` value to a pointer variable if you do not have the exact address to be assigned
- This is done at the time of variable declaration, as in `type *pointer_name = NULL;` (or the quivalent version `type *pointer_name = 0;`)
- A pointer that is assigned to `NULL` is called a null pointer
- In most operating systems programs are not permitted to
access memory at address 0
- If a pointer contains the `NULL` (or zero) value, it is assumed to
point to nothing

Example:

```c
#include <stdio.h>

int main() {
    int *ptr = NULL;
    printf("The value of ptr is: %p \n", ptr);

    return 0;
}
```

Output:
```bash
The value of ptr is: (nil)
```

## 5 Pointer arithmetic

Let's go back to printing all three elements of the array called `array` introduced earlier.

```c
int *array_ptr = array;
printf(" first element: %i\n", *array_ptr++);
printf("second element: %i\n", *array_ptr++);
printf(" third element: %i\n", *array_ptr);
```

Output:

```bash
First element of array: 42
Second element of array: 77
Third element of array: 89
```

In case you’re not familiar with the `++` operator: it adds 1 to a variable, the same as `variable += 1` (remember that because we used the postfix expression `array_ptr++`, rather than the prefix expression `++array_ptr`, the expression evaluated to the value of `array_ptr` from _before_ it was incremented rather than after).

If you are not familiar with pre/post-fix expressions:

:::{note}

```c
int i = 10;   // (1)
int j = ++i;  // (2)
int k = i++;  // (3)
```

For a pre-fix expression `int j = ++i;`, the variable `i` is incremented by one (`i = i + 1`, so `i = 11`), and then it's deep copied to `j` (so `j = 11`). For the post-fix expression int `k = i++`;, the variable `i` (which is still 11), is first deep copied to `k` (so `k = 11`), and then `i` is incremented by one (so `i = 12`)
:::

Going back to our example, the type of a pointer matters. The type of the pointer here is `int`. When you add to or subtract from a pointer, the amount by which you do that is multiplied by the size of the type of the pointer. In the case of our two increments, each one that you added was multiplied by `sizeof(int)`.

We could have printed the values in the array in a for-loop:

```{literalinclude} ../c_programs/module2-2_c_pointers/print_array.c
:language: c
:linenos: false
```

Output:

```bash
Address of array [0] = 0x7ffd1f9a773c
Value of array [0] = 42
Address of array [1] = 0x7ffd1f9a7740
Value of array [1] = 77
Address of array [2] = 0x7ffd1f9a7744
Value of array [2] = 89
```

## 6 Indexing

To print the first entry of the array `array` we can use:
```c
printf("%i\n", array[0]);
```
which will show the value `42`. Technically, the index operator (e.g. `array[0]`) has _nothing to do with arrays_, even though it is its most common usage. Remember that arrays decay to pointers. That’s a _pointer_ you passed to that operator, not an array.

One could do:

```c
int array[] = {42, 77, 89};
int *array_ptr = &array[1];
printf("%i\n", array_ptr[1]);
```

What happened is displayed in the following diagram

![](../img/array_indexing.png)


`array` points to the first element of the array; `array_ptr` is set to `&array[1]`, so it points to the second element of the array. So `array_ptr[1]` is equivalent to `array[2]` (`array_ptr` starts at the second element of the array `array`, so the second element of `array_ptr` is the third element of the array `array`).

Also, you might notice that because the first element is `sizeof(int)` bytes wide (being an `int`), the second element is `sizeof(int)` bytes forward of the start of the array. You are correct: `array[1]` is equivalent to `*(array + 1)`. (Remember that the number added to or subtracted from a pointer is multiplied by the size of the pointer’s type, so that `1` adds `sizeof(int)` bytes to the pointer value.)

## 7 Indirection or pointer to pointer

- A pointer to a pointer is a form of multiple indirection, or chain of pointers
- Normally, a pointer contains the address of a variable
- When one defines a pointer to a pointer, the first pointer contains the address of the second pointer, which points to the location that contains the actual value
- A variable that is a pointer to a pointer must be declared as such. Place an additional asterisk in front of its name
- When a target value is indirectly pointed to by a pointer to a pointer, accessing that value requires that the asterisk operator be applied twice

Example:

```{literalinclude} ../c_programs/module2-2_c_pointers/pointer_indirection.c
:language: c
:linenos: false
```

Output:

```bash
Value of var = 3000
Value pointed to by ptr: *ptr = 3000
Value pointed to by pptr: ∗∗pptr = 3000
Address of var is 0x7ffce4c71174
Address of ptr is 0x7ffce4c71178
Address of pptr is 0x7ffce4c71180
Value of ptr is 0x7ffce4c71174
Value of pptr is 0x7ffce4c71178
```

Dereferencing a pointer-to-pointer works in the same way. Example:

```c
int    a =  3;
int   *b = &a;
int  **c = &b;
int ***d = &c;
```

```c
*d == c; // Dereferencing an (int ***) once gets you an (int **)
**d ==  *c ==  b; // Dereferencing an (int ***) twice, or an (int **) once, gets you an (int *)
***d == **c == *b == a == 3; // Dereferencing an (int ***) thrice, or an (int **) twice, or an (int *) once, gets you an int
```




# Translation from OBJEling to IMP

Consider the OBJEling class defined by the following code
```c++
class point {
  attribute int x;
  attribute int y;

  method void constructor(int a, int b) {
    this.x = a;
    this.y = b;
  }

  method int sum(int z) {
    return z + this.x + this.y;
  }
}

function void main() {
  var point p;
  var int c;
  p = new point(2, 1);
  c = p.sum(48);
  putchar(c);
}
```

## Data and class representation

### Object instance layout

An instance of the class `point` is represented by a pointer to a heap block with three fields:

1. a pointer to the class descriptor of `point` (offset 0)
2. the value of `x` (offset 4)
3. the value of `y` (offset 8)

|address offset| data |
|:------------:|:----:|
| 0 | descriptor |
| 4 | x |
| 8 | y |

### Class descriptor layout

The class descriptor is defined by a heap block with three fields:
1. a `null` pointer, since the class has no parent (offset 0)
2. a code pointer to a function `point_constructor` (offset 4)
3. a code pointer to a function `point_sum` (offset 8)

|address offset| data |
|:------------:|:----:|
| 0 | null |
| 4 | constructor_ptr |
| 8 | sum_ptr |

Note that each method is mapped to a function whose name combines the
name of the class and the name of the method (hence `point_sum`).
We also define a global variable `point_descr` containing a pointer
to the class descriptor of the class `point`.


## Translation of an assignment instruction

Consider the OBJEling instruction
```c++
p.x = 3*p.x;
```

### A. Representation in objlang

The fragment `p.x` is an access to a field.
objlang AST for this fragment is written as follows in caml (not showing type 
annotations):
```OCaml
Atr(Var "p", "x")
```


This fragment denotes a memory location. It can be used for __reading__ from or
__writing__ to memory, using expression constructor `Read` or instruction
constructor `Write`. objlang AST for the instruction 
```c++
p.x = 3 * p.x; 
```
is then (omitting types):

``` OCaml

let px = Atr(Var "p", "x") in

Write(
  px,
  Binop(
    Mul, 
    Cst 3,
    Read px
  )
)
```

### B. Translation to IMP

The address of the memory location denoted by `p.x` can be obtained by
adding to the pointer `p` an offset corresponding to the attribute `x`,
that is 4.

The fragment is translated
into IMP as _(concrete IMP syntax)_:
```c++
  p + 4
```
or _(abstract IMP syntax)_
``` OCaml
  Binop(Add, Var "p", Cst 4)
```
abbreviated __ma'__ below

For the remainder of the instruction: the `Read` access is translated using
a `Deref` expression, and the `Write` access is translated using a `Write` instruction.

Thus, objlang instruction 
```c++
p.x = 3 * p.x
```
translates to IMP _(concrete IMP syntax)_:
```c++
*(p+4) = 3 * (*(p+4));
``` 
or _(abstract IMP syntax)_
``` OCaml
Write(ma', Binop(Mul, Cst 3, Unop(Read, ma')))
```

## Translation of a method call

Consider the objlang expression 
```c++
p.sum(48)
```

objlang AST is (omitting types):

```OCaml
MCall(Var "p", "sum", [Cst 48])
```

It comprises:
1. an access to the objet `p`;
2. a method name;
3. a list of 1 argument;

The method call translates to IMP with a dynamic function call _(function called
through a pointer rather than a name)_, with the two parameters `p` and `Cst 48`.

If we write `f` the function pointer, this gives:
```c++
  *f(p, 48)                     // concrete IMP syntax
```
or
```OCaml
  DCall(f, [Var "p", Cst 48])   // abstract IMP syntax
```

Now, focus on this `f` pointer: the code pointer of `sum` is stored in the 
class descriptor of `point`, with an offset of 8 bytes. We get the pointer
this way:

1. Address of the object: `p`
2. Contents of the first field (header): `*p`,
this contents is the address of the class descriptor
3. Address where `sum` is stored: `*p+8`
4. The address of `sum` itself: `*(*p+8)` (what we are looking for)

In IMP abstract syntax:
``` OCaml
Deref(Binop(Add, Deref(Var "p"), Cst 8))
```

Full methode call in IMP:
``` c
*(*p+8)(p, 48)      // concrete IMP syntax
```
or _(abstract IMP syntax)_
``` OCaml
DCall(
  Deref(
    Binop(
      Add,
      Deref(Var "p"),
      Cst 8
    )
  ),
  [Var "p", Cst 48]
)
```

## Translation of new object creation

Consider the objlang instruction
```c++
p = new point(2, 1);
```

In objlang abstract syntax (omitting types), we provide the class name and a
list of arguments:

```OCaml
Set("p", New("point", [Cst 2; Cst 1]))
```

Object creation is in three steps:
1. first allocating the block;
2. initializing its header;
3. finally calling the `constructor` method on
the created object.

Using the variable `p` to store the created object, we would have the following three IMP instructions:
```c++
p = alloc(12);
*p = point_descr;
point_constructor(p, 2, 1);
```

In IMP abstract syntax, ommiting types:
``` OCaml
Seq [
  Set("p", Alloc(Cst 12));
  Write(Var "p", Var "point_descr");
  Expr(
    Call(
      "point_constructor",
      [Var "p"; Cst 2; Cst 1]
    )
  )
]
```

## Creation of the class descriptor

The class descriptor is created before the execution of the actual main
code of the program. It is not described by any specific objlang instruction:
it is just triggerer by the existence of a class.

Creating the class descriptor requires creating a block, initializing the
global variable `point_descr` with the address of this block, and storing
in the second and third fields the code pointers for the methods. We also store 0 in the first field to indicate the absence of a parent class.

In IMP syntax, we would have:
```c++
  point_descr = alloc(12);
  *point_descr = 0;
  *(point_descr+4) = &point_constructor;
  *(point_descr+8) = &point_sum;
```

In IMP abstract syntax (omitting types):
```OCaml
[
  Set(Var "point_descr", Alloc(Cst 12));
  Write(Var "point_descr", Cst 0);
  Write(
    Binop(Add, Var "point_descr", Cst 4),
    Addr "point_constructor"
  );
  Write(
    Binop(Add, Var "point_descr", Cst 8),
    Addr "point_sum"
  )
]
```

## Required extension of IMP

The proposed translation requires extending IMP abstract syntax with **3** new elements.

``` OCaml
type expression =
  | Addr of string
  | DCall of expression * expression list
  | ...

type instruction = 
  | Seq of sequence
  | ...
```

### Address of static element
``` OCaml
Addr of string
```
It applies to the name of a statically allocated element (e.g. the name of a function), and returns its address.

### Dynamic call
``` OCaml
DCall of expression * expression list
```
It represents a "dynamic" call, that is a call made using a function pointer.

The first argument is an expression that computes this pointer, and the list contains the arguments (as it is the case for `Call`).

### Translate multiple instructions
``` OCaml
Seq of sequence
```
This constructor is convenient when a __single__ objlang instruction translates to (a sequence of) more than one Imp instructions.

Do not forget to add these in `imp.ml`, and to extends `imp2mips.ml` and `imppp.ml` accordingly. This folder however already contains the updated abstract syntax and parser for objlang.

## Roadmap

### Base

Your first goal is to compile a version of objlang that obeys to the following restrictions:
1. no inheritance
2. method calls, functions calls, and object creation only allowed as "top-level" expressions
The latter means that
```c++
  p = new point(2, 1);
  s = p.sum(48);
```
are allowed, but that
```c++
  x = (new point(2, 1)).x;
  y = 1 + p.sum(48);
```
are not since the "new" and the call are subexpressions of the main expression.
Note that this restricted version is enough for dealing with the class `point`.

Without inheritance, each class is defined only by the __attributes__ and __methods__ explicitely declared in its definition. Then:

- the number of fields of the memory block that represents an object `obj` of some class `C` can be computed straightforwardly as `(1 + number)` of attributs in the definition of `C` (and the same for the class descriptor with respect to the number of methods)

- the offset for accessing an attribute `x` is computed using the position of `x` in the list that enumerates the attributes of the class (and the same for the offset of a method in a class descriptor) do not forget to skip the header when computing the offset.
      
- a class descriptor contains only the methods explicitely defined in the corresponding class.

With complex expressions such as calls restricted to toplevel, they can be
treated by the function translating instructions, thus keeping the nice types
``` OCaml
val tr_instr: Objlang.instruction -> Imp.instruction
val tr_expr:  Objlang.expression  -> Imp.expression
```

Thus you can ignore the cases `New`, `Call` and `MCall` in `tr_expr` and consider cases such as `Set(x, New(...))` in `tr_instr` instead.

Remark that you can implement this base version using the full AST: just
ignore the field `parent` of class definitions, and fail if a call or a new appears at the wrong place.

Note on typing: the typing module `objlangtyper` is similar to the one built for SIMP, with one novelty: whenever typing a method, the environment should contain a local variable `_this`, whose type is the current class. You can add it to the local environment, or make its type an explicit additional parameter of the typing functions. To keep the same code for typing __methods__
where `_this` exists and functions where `_this` does not exist, you may use a dummy type in the latter case.

If you have a working version of this base compiler, you can consider a few
extensions:
- A. inheritance
- B. preprocessing calls
- C. advanced typing (relies partly on A)

Note: this is a list of suggestions. It is not intended to be restrictive. The suggested extensions may vary in difficulty.

## Extension A. Inheritance

The goal of this extension is to allow the use of `extends` in a class definition of an objlang program, for defining a child class that inherits
attributes and methods from a parent class. The child class may also:
1. add new attributes
2. add new methods
3. overwrite methods of the parent class

If you consider implementing this extension, I suggest you proceed again following an incremental approach
1. add __new attributes__
2. add __new method__
3. and __overwrite method__

### 1. New attributes

Novelty with respect to the base version: one has to take into account the
number of attributes of the parent class for deducing the total number of
attributes of the child class and their offsets.

|address offset| data |
|:------------:|:----:|
| 0 | descriptor |
| 4 | inherited attributes |
| ? | new attributes |

However, since there is no new method, you could imagine sharing the class descriptor with the parent class.

Note that in practice, you probably would like to add a dedicated constructor.

However, in simple cases this particular method is only called statically and thus is never used through the descriptor, you can indeed at first avoid the creation of a new descriptor just for that.

### 2. New methods

As in the previous extension, one has to take into account the number of methods of the parent class to compute the right sizes and offsets.

Do not forget to also build a new descriptor that contains the methods of both the parent and the child class.

### 3. Overriding

When a child class `C'` redefines a method `m` already defined by its base class `C`, the new definition `C'_m` should override the old definition `C_m` in the class descriptor of `C'`. Thus:
- method `C'_m` should have the same offset as `C_m`
- the class descriptor for `C'` should contain a pointer to the right version of `m`.

### 4. Bonus : super
  
When methods can be overriden, the keyword [super] can become useful for 
accessing the parent version of some method.


## Extension B. Preprocessing calls

Restricting calls and object creations to toplevel expressions makes the
translation cleaner, but restricts the allowed input programs. However, it is
possible however to enable again the full syntax of expressions, while keeping
the current state of `objlang2imp`. For this, you can add a preprocessing pass
applied between `objlangtyper` and `objlang2imp`, which takes as input an
arbitrary objlang (typed) AST, and returns an equivalent program where calls
and object creation are at toplevel.

For this, it is enough to translate each call that is not already at toplevel by an assignment to some new variable, and then refer to the variable.
For instance,
``` c++
  x = p.sum(24) + (new point(2, 1)).sum(12);
```
could be "flattened" as
```c++
  a = p.sum(24);
  b = new point(2, 1);
  c = b.sum(12);
  x = a+c;
```

This translation decomposes complex expressions, at the cost of introducing a
series of short-lived additional variables.

However, with a proper register
allocation this additional cost will reduce to zero.


## Extension C. Advanced typing

Here are a few suggestions of extensions related to typing. The first ones
can be implemented on the base version, some others make sense only when
inheritance is already present.

### Helpful message
Produce a helpful message when the typer uncovers a typing error.

For instance: in case of an attempt to access an attribute that does not exist in the considered class, produce a message that names the __class__ and the missing __attribute__

### Instanceof
Add an expression `obj instanceof C`, that tests whether the object `obj` is from a class `C'` that is a subclass of `C` (or `C` itself)

### Type casting
Allow casting an object `obj` of static type `C` to another static type `C'`.

__Reminder__: casting toward a parent class is always possible, casting toward a child class requires a runtime check, and casting toward an unrelated class is not allowed

### Static overloading
Allow static overloading of methods. For each method, build a new name based on the method name and the types of its argument.

For each call, use the types computed for the arguments to select the right method.

__Warning__: static overloading + inheritance and implicit up-cast produce a few corner cases where a call might become ambiguous; you may ignore this at first.

### Abstract class

Allow the definition of abstract classes, which has abstract methods.
1. one cannot create an instance of such a class with `new`. 
2. Child classes should eventually provide definitions for all abstract methods
3. the offset of each method is fixed by the abstract class that first introduces it.
4. the offset is the same in any child class that defines the method

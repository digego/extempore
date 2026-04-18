---
title: Generics
---

## TODO

Add something about alloc/zalloc etc with generics.

## Introduction

In an earlier chapter we saw how you could overload functions so that they could handle different types appropriately:

```xtlang
(bind-func sum:[i64,i64,i64]*
  (lambda (x y)
    (+ x y) ))

(bind-func sum:[double,double,double]*
  (lambda (x y)
    (+ x y) ))
```

While this is useful, it's also a lot of unnecessary work as we're rewriting the same function twice. In this chapter we're going to show you a better way using generics.

NOTE: Currently the generics implementation in xtlang has slow compile times. This is a known problem which is being worked on. They still work just fine once they are compiled.

## Generic Functions

When writing a generic function we use generic types, where a generic type is a placeholder for our eventual concrete type. We write a generic type using `!`. So for example:

```xtlang
(bind-func gsum:[!a,!a,!a]*
  (lambda (x y) (+ x y)))
```

This function can take any type, the only restriction is that both parameters and the return type _must share that type_. For example:

```xtlang
($ (gsum 3 4)) ;; OK
($ (gsum 3.4 5.3)) ;; OK

($ (gsum 3 4.3)) ;; You will get a type error
($ (let ((x:double (gsum 3 4))) ;; you will get a type error
      (println x)))
```

We could even make this function even more generic by using convert [fn::in real code this particular example is best avoided. It's usually better to explicitly cast in this situations to prevent unexpected errors]:

```xtlang
(bind-func rgsum:[!b,!a,!a]*
  (lambda (x y) (convert (+ x y))))

($ (let ((x:double (gsum 3 4))) ;; this now works
      (println x)))
```

So here we've specified a second type by simply using `!b`. Whenever you have a different generic type you simply introduce a new character. For example: `my-func:[!c,!a,!b,!b]*`. `my-func` is a function that takes three parameters. The first parameter has type: `!a`, while the other two parameters are of type `!b`. This function then returns a parameter that has type `!c`. So the following calls are all supported:

+ `(let ((x:i64 (my-call 3.2 'a' 'b'))) x)`
+ `(let ((x:i64 (my-call 'c' 'a' 'b'))) x)` - `!b` and `!a` can share the same type.
+ `(let ((x:i64 (my-call 3 2 4))) x)` - `!a` and `!b` and `!c` can all share the same type

but the following will result in compiler errors:

+ `(let ((x:i64 (my-call 3.2 'a' 3))) x)` - the last two parameters must share a type.

We can also create functions that mix abstract and concrete types:

```xtlang
(bind-func my-func:[i64,!a,!a]*
  (lambda (x y) (convert (+ x y))))

($ (my-func 3.2 4.5)) ;; 7
($ (my-func 3 4)) ;; 7
```

Returning to our `gsum` function - what happens if we pass in a type that is not supported by the function `+`?

```xtlang
($ (gsum 'c' 'd')) ;; This won't compile
```

You will have seen a compiler error that looks like:
`Compiler Error  bad or unsupported atom type ast: #f`

What a horrible error! There is absolutely nothing that tells us what has gone wrong. As we'll see later in this chapter there are some things that we can do about this, and better tools are coming, but for the moment this is a limitation we have to live with. If you're writing a library, make sure you document the requirements for your generic function.

## Behind the Scenes

When we create a generic function the compiler doesn't actually generate a function. Instead a generic function is a _template_ for a function which can be filled with concrete types. It is only when we supply concrete types that the xtlang compiler tries to generate a function.

So for example:

```xtlang
;; No functions are generated when you evaluate this
(bind-func gen-sum:[!a,!a,!a]
  (lambda x y) (+ x y))
```

no functions have been compiled yet. Let's call gen-sum with a concrete i64 type: `($ (gen-sum 3 4))`. The compiler looks to see if the function `gen-sum:[i64,i64,i64]*` exists. It can't find it, so now it uses the `gen-sum` _template_ to compile it. Once `gen-sum:[i64,i64,i64]*` has been compiled, the call can be evaluated.

```xtlang
($ (gen-sum 3 4)) ;; This concrete instance has already been compiled.

($ (gen-sum 3.3 4.4)) ;; This concrete instance needs to be compiled first before we can use it
```

This means that the first time you call a generic function with a new type signature you will need to wait for the function to be compiled (which is currently quite slow, though this is being worked on).

This means that generic functions aren't quite like concrete functions:

```xtlang
;; the compiler will complain that it cannot find 'summy'
(bind-func sumf:[i64,i64,i64]*
  (lambda (x y) (summy x y)))

;; the compiler has no problems with this
(bind-func sumg:[!a,!a,!a]*
  (lambda (x y) (summy x y)))

;; but now it will complain as it tries to compile sumg:[i64,i64,i64]*
($ (sumg 3 4))
```

The compiler has no problem with you referencing undefined functions and variables in generic functions. You will only see errors when you try to use that function with concrete types. It is up to you to make sure that your functions/variables exist, and that they work for all the types supported by your generic function.

## Generic Types

Let's suppose we want to create a list, but we don't care what the list contains:

```xtlang
(sys:load "libs/base/adt.xtm")

(bind-data MyList{!a}
           (MyNil)
           (MyCons !a MyList{!a}*))
```

Note that this is exactly like an ADT with concrete types, but instead we're specifying our abstract type using `!a`. Now let's do some useful with this list. First of all we need to be able to see what's in a list:

```xtlang
(bind-func toString_help:[String*,MyList{!a}*,String*]*
  (lambda (lst s)
    (MyCons$ lst (x xs)
           (MyNil$ xs ()
                 (toString_help xs (cat s (toString x)))
                 (toString_help xs (cat s (toString x) (Str ","))))
           (cat s (Str "]")))))

(bind-func print:[void,MyList{!a}*]*
  (lambda (lst)
    (printout (toString_help lst (Str "[")))
    void))

(bind-func toString:[String*,List{!a}*]*
  (lambda (lst)
    (toString_help lst (Str "["))))

(bind-val a-list MyList{i64}* (MyCons 1 (MyCons 2 (MyCons 3 (MyCons 4 (MyCons 5 (MyNil)))))))

($ (println a-list)) ;; this prints out '[1,2,3,4,5]'
```

Defining functions for an abstract ADT is almost the same as with concrete ADTs, the only difference is that that we add `{!a}` to the end of our type: `List{!a}`.

When we have a concrete instance of the type then the concrete type goes inside the brackets. For example: `a-list:MyList{i64}*`.

Now let's do something a little bit more exciting, we're going to write a `fmap` function for our list.

A `fmap` function is a higher order function that allows us to apply a function to every element in our list. So for example:

```xtlang
($ (map (lambda (x) (+ x 3)) a-list)) ;; this will add 3 to every member of a-list
```

It doesn't matter what function we pass in, so long as it has the right function signature. By using generics we can make sure that the signature is pretty generic. So how do we define `fmap`. Firstly `fmap` should take two parameters.

The first parameter is a function that takes a single parameter that must share the type of our list: `fn:[!b,!a]*`. Note that the return type is different to the source type. There's no reason why we shouldn't be able to take a list of integers, process them and return a list of strings.

The second parameter of course is the list. So let's put this together and define a type: `fmap:[MyList{!b}*,[!b,!a]*,MyList{!a}*]`

In other words we take a list of type `MyList{!a}`, a function of type `[!b,!a]*` and return a list of type `MyList{!b}*` (remember that we typically pass pointers to complex types).

So let's write this function:

```xtlang
(bind-func fmap:[MyList{!b}*,[!b,!a]*,MyList{!a}*]*
  (lambda (f lst)
    (MyCons$ lst (x xs) (MyCons (f x) (fmap f xs)) (MyNil))))

($ (println (fmap (lambda (x) (+ 3 x)) a-list))) ;; '[4,5,6,7,8]'
($ (println (fmap (lambda (x) (toString x)) a-list))) ;; fmap returns a list of strings.
```

This allows us to very succintly write a library of generic data types with useful functions, and this is in fact how the prelude library is written in xtlang.

One more note about generics and complex data types. You can of course mix concrete and generic types in your type definitions:

```xtlang
(bind-data EitherT{!a}
           (LeftT String)
           (RightT !a))

(bind-type pointT <!a,!a,i64>) ;;
```

## Constraints

NOTE: Constraints are currently scheme. Presumably this is going to change?

Often when we're writing a function we want it to be generic, but not too generic. We saw this above where we wrote the function `gsum`. `gsum` will only work for types that support addition, but there is nothing in the type signature that makes this explicit[fn:: One solution to this is typeclasses which may be coming in the future, but currently this isn't available.]. When we try to compile the function we get a rather obscure error.

One thing that we can do is use a type constraint:

```xtlang
(bind-func square:[!a,!a]* -> (lambda (ret x) (t:number? x))
  (lambda (x)
    (* x x)))
```

The function that comes after the `->` is the constraint. Let's look at it a bit more closely:
`(lambda (ret x) (t:number? x))`. When you call a generic function with types that the compiler hasn't seen before, the compiler will call this constraint to see if this function can be generalized to the new type. Let's look at this in operation:

```xtlang
($ (square 4))

;; the compiler hasn't seen square:[i64,i64]* before
;; so it needs to compile a version of this function.

;; before compilation can happen, it needs to check the constraint:
;; So the type of our parameter is passed to (lambda (ret x) t:number? x)
;; where x is the type i64.
;; i64 is indeed a number type, so the function returns #t.
;; we can compile square:[i64,i64]*

($ (square 5))
;; the compiler has already generated a version of square:[i64,i64]* so
;; there is no need to check the constraint.

($ (square 5.5))
;; square:[double,double]* needs to be compiled, so again the constraint
;; is checked. And again this passes the constraint so the compiler generates
;; this function.

($ (square 'c'))
;; square:[i8,i8]* needs to be compiled so the constraint is checked
;; t:number? returns #f when it is passed an i8 (or character) so compilation fails
;; and we get the following error:

Constraint Error square failed constraint (lambda (ret x) (t:number? x))
with type: [String*,String*]*
ast: (square (String##15 "c"))
```

So our constraint is a predicate which is passed type information and returns true or false. And we can do this for other functions. Here's gSum from earlier:

```xtlang
(bind-func gSum:[!a,!a,!a]* -> (lambda (ret x y) (t:number? x))
  (lambda (x y)
    (+ x y)))

($ (gSum 3 4)) ;; 7
($ (gSum 3.2 4.3)) ;; 7.5
($ (gSum 'n' 'a')) ;; won't compile
```

And we can make it even more generic (note this is only for the sake of having an example. This would be a bad idea in practice):

```xtlang
(bind-func vgSum:[!b,!a,!b]* -> (lambda (ret x y) (and (t:number? x) (t:number? y)))
  (lambda (x y)
    (+ (convert x) (convert y))))

($ (vgSum 3 4.5))
```

### Type Predicates

When writing a constraint you have access to the following type predicates:

+ `t:integer?` - is this type an integer?
+ `t:integer?` - is this type an integer?
+ `t:float?` - is this a floating point type (e.g. a double, or float)
+ `t:number?` - is this a number?
+ `t:void?` - is this type a void? Can be useful if you want to make sure that none of your types are voids.
+ `t:signed?` - TODO Not sure
+ `t:closure?` - is this type a closure?
+ `t:vector?` - is this type a vector?
+ `t:array?` - is this type an array?
+ `t:tuple?` - is this type a a tuple?
+ `t:scalar?` - TODO Not sure.
+ `t:pointer?` - is this type a pointer?
+ `t:elts?` - Takes two parameters. The type and a number 'num'. Returns true if the type has num elements (for a data type) or num parameters (for a function).
+ `t:closure?` - Takes two parameters. The type and a number 'num'. Returns true if the type is a closure with num parameters.
+ `t:named?` - Does this type have a name (through `bind-type`).

Currently the type predicates are still a work in progress. But soon you will be able to easily write constraints that allow you to make sure that arrays have a particular size, and do quite complex structure analysis. This is a part of xtlang which is currently quite cutting edge so expect changes here.

### Defining Functions that Enforce Dependencies

Let's say that we have an overloaded function `sqr`:

```xtlang
;; define two sqr polys (i32 and i64)
(bind-func sqr (lambda (x:i32) (* x x)))
(bind-func sqr (lambda (x:i64) (* x x)))
```

and we want to write a generic function that works for any type that supports this function. We can do this using the `t:poly-exists?` function:

```xtlang
(bind-func sqr2:[!a,!a]* -> (lambda (r x) (t:poly-exists? 'sqr `(,x ,x)))
  (lambda (x)
    (sqr (sqr x))))
```

t:poly-exists? takes two parameters. A symbol or string for the function name and a form for the parameters. So in this case the list ``(,x ,x)` tells us that this function will only work with versions of sqr that have the type `[!a,!a]*`.

### Overloading Generic Functions with Constraints

When the compiler tries to find the function that matches your call it starts by looking for the most specific match. So if you call `(my-func 3 4)` it will begin by looking for a compiled function with the signature `my-func:[i64,i64,i64]*`. If it can't find a matching function it will then try to generate it using any generic functions that it knows about. We can use this fact, along with constraints, to build functions that are overloaded based upon the type of the parameters:

```xtlang
(bind-func silly-func:[!a,!a]* -> (lambda (ret x) (t:integer? x))
  (lambda (x)
    (* x x)))

(bind-func silly-func:[!a,!a]* -> (lambda (ret x) (t:float? x))
  (lambda (x)
    (sqrt x)))

(bind-func silly-func:[!a,!a]* -> (lambda (ret x) (t:pointer? x))
  (lambda (x)
    (println (cast x i8*))
    x))
```

So here we have a function that will square integers (`i8`,`i16`,`i32`,`i64`), get the square root of floating point types (`float`,`double`) and to otherwise print the address of a pointer type.

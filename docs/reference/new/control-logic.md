---
title: Branching and Looping
---

## Introduction

In this chapter we'll discuss some of your options in xtlang for control logic and how to use them. We'll begin by looking at functions that you are probably more familiar with, before looking at ways in which you can use tail recursion to achieve some of the same aims.

## Blocks

### begin

The `begin` form allows you to combine multiple expressions. The final expression in the block is the return value (all other return values are ignored.)

```xtlang
;; This won't compile
($ ((+ 3 5)
    (- 3 4)))

;; This will and returns -1
($ (begin
    (+ 3 5)  ;; return value will be ignored.
    (-3 4)))
```

This is usually most useful for sequencing expressions with side effects. For example:

```xtlang
($ (begin
     (println "line 1")
     (println "line 2")))
```

Or if you wanted to add logging to a function:

```xtlang
;; Dumb Logging Code
(bind-func log-adder
  (lambda (x:i64 y:i64)
    (begin
      (println "x:" x)
      (println "y:" y)
      (println "answer:" (+ x y))
      (+ x y))))

($ (log-adder 3 4))
```

## Predicates

Predicates are functions that either return #t, or #f. These are mostly self-explanatory, and so this section will mostly just document them for reference purposes.

While predicates in xtlang work with many different types, they expect both types to be identical. So for example while both `(< 3 5)` and `(< 3.5 5.6)` are acceptable, `(< 3.5 5)` will throw a compiler error. If you need to compare two variables of different types then you will need to coerce one of them using either `convert`, or one of the more specific coercion functions. When doing this always be careful to coerce to a datatype that can hold more information, unless you _really_ know what you're doing. For example:

```xtlang
($ (= 3.1 (convert 3))) ;; returns #f

($ (= (convert 3.1) 3)) ;; returns #t, NOT #f
```

The second expression results in a subtle bug. If you want to 'demote' a variable to a datatype that holds less information, always be explicit about how you want the information to be removed. E.g. if converting a float to an integer, use `ceil`, `floor`, `round`, etc to make explict the type of conversion you are expecting:

```xtlang
($ (= (convert (abs 2.9)) 3)) ;; returns #t, which is what we are expecting.
```

### Greater Than

+ __Function:__ `>`
+ __Supported Types:__ `i32`, `i64`, `float`, `double`, `i8`, pointers?

```xtlang
($ (> 3 4))  ;; #f
($ (> 4 3))  ;; #t
```

### Greater Than or Equals

+ __Function:__ `>=`
+ __Supported Types:__ `i32`, `i64`, `float`, `double`, `i8`, pointers?

```xtlang
($ (>= 3 4))  ;; #f
($ (>= 4 3))  ;; #t
($ (>= 4 4))  ;; #t
```


### Less than

+ __Function:__ `<`
+ __Supported Types:__ `i32`, `i64`, `float`, `double`, `i8`, pointers?

```xtlang
($ (< 3 4))  ;; #t
($ (< 4 3))  ;; #f
```

### Less Than or Equals

+ __Function:__ `<=`
+ __Supported Types:__ `i32`, `i64`, `float`, `double`, `i8`, pointers?

```xtlang
($ (<= 3 4))  ;; #t
($ (<= 4 3))  ;; #f
($ (<= 4 4))  ;; #t
```

### Equals

+ __Function:__ `=`
+ __Supported Types:__ `i32`, `i64`, `float`, `double`, `i8`, pointers?

```xtlang
($ (= 3 4))  ;; #f
($ (= 4 4))  ;; #t
```

### Not Equal

+ __Function:__ `<>`
+ __Supported Types:__ `i32`, `i64`, `float`, `double`, `i8`, pointers?

```xtlang
($ (<> 3 4))  ;; #t
($ (<> 4 4))  ;; #f
```

### Pointer is Null?

+ __Function:__ `null?`
+ __Supported Types:__ pointers?

```xtlang
TODO
```

### Not a Number?

Returns true if the value passed to it is not a number (e.g. infinity).

+ __Function:__ `TODO`
+ __Supported Types:__ `i32`, `i64`, `float`, `double`, `i8`

Might need to be implemented...

```xtlang
TODO
```

### OR

Takes two predicates and applies 'OR' to their values.

+ __Function:__ `or`
+ __Supported Types:__ `i1`

```xtlang
($ (or #t #f )) ;; #t
($ (or (< 3 4) (= 3 4))) ;; #t
```

### AND

Takes two predicates and applies 'OR' to their values.

+ __Function:__ `and`
+ __Supported Types:__ `i1`

```xtlang
($ (and #t #f )) ;; #f
($ (and #t #t )) ;; #t
($ (and (< 3 4) (= 3 4))) ;; #f
```

### NOT

Takes a predicate and inverts its value.

+ __Function:__ `not`
+ __Supported Types:__ `i1`

```xtlang
($ (not #t )) ;; #f
($ (not #f )) ;; #t
($ (not (< 3 4)) ;; #f
```

## Branching Logic

### if

You have already seen `if` used already, and so we'll just formally describe here how it works. If takes three parameters:

1. An expression, or function, that returns true or false (i.e. a type of `i1`).
1. The branch to evaluate if the comparison is true.
1. The branch to evaluate if the comparison is false (e.g. the 'else' statement). This expression must have the same return type as the other branch. This return type can of course be `void`.

```xtlang
(bind-func simple-if
  (lambda (x:i1)
    (if (x)
        (println "x is true")
        (println "x is false"))))

($ (simple-if #t)) ;; "x is true"
($ (simple-if #f)) ;; "x is false"
```

Remember that both parts of the if statement must have the same return type:

```xtlang
;; This won't compile as 5:i64 and 6.5:double
(bind-func return-if
  (lambda (x:i1)
    (if (x) 5 6.5)))

;; this is fine as 5 and 6 both have type i64
(bind-func return-if
  (lambda (x:i1)
    (if (x) 5 6)))
```

### cond

Where `if` only allows two branches, `cond` supports multiple branches, with an optional default. It takes the form:

```xtlang
(cond
 ((comparison-1) (branch-1))   ;; this is the first branch
 ((comparison-2) (branch-2))   ;; this is the second branch
                     ...
 ((comparison-n) (branch-n))   ;; the nth branch
 (else (default-branch))       ;; an optional default
)
```

Only one branch will ever be taken, and as soon as a branch is completed then the `cond` block will return with the value from that branch.

Each branch must return the same type, otherwise the compiler will complain (the type can of course be void). If you do not provide an else statement then the compiler will make sure that _a_ value of the right _type_ is returned as a default, though no guarantees are provided as to what value will be.

So let's look at a couple of examples. First a version that returns default:

```xtlang
($ (cond
    ((= 5 4) (println "never reached"))
    ((= 4 4) (println "4 equals 4"))
    ((= 5 5) (println "never reached"))))  ;; this branch will never be executed.
```

If we were to modify this code slightly then nothing will happen:

```xtlang
;; Nothing will be printed to the terminal
($ (cond
    ((= 5 4) (println "never reached"))
    ((= 4 6) (println "4 equals 4"))
    ((= 5 8) (println "never reached"))))
```

::: info
If your return type is void then not providing an `else` branch often makes sense. For all other types this is likely to lead to subtle errors and is best avoided. If you think that the else statement should never be reached then put a logging statement there so you know that something went wrong.
:::
::: info
If we add and `else` statement we can add defaults:
:::

```xtlang
;; prints 7 to the terminal
($ (println
     (cond
       ((= 5 4) 55)
       ((= 4 6) 46)
       (else 7))))
```

## Looping

### while

`while`, unsuprisingly, loops until it's predicate is false. If takes two parameters:

1. An expression, or function, that returns true or false (e.g. an `i1` type).
1. The expression to evaluate while the predicate is `true`.

```xtlang
(bind-func simple-while
  (lambda (x:i64)
    (let ((count 0))
      (while (> x 0)
        (begin
          (inc count 1)
          (dec x 1)))
      (println "count is" count))))

($ (simple-while 4)) ;; "count is 4"
```

### until

Someone should write this as it's very useful... :)

### dotimes

`dotimes` repeats the expression a fixed number of times. It takes the following form:

```(dotimes (parameters) (expression))```

The parameters block consists of:

1. __REQUIRED:__ A variable to use as a counter. This must be defined in the environment of the `dotimes` loop (e.g. by using a `let` statement).
2. __REQUIRED:__ The number of times to loop.
3. __OPTIONAL:__ The starting value for the variable. The default for this value is `0`.

```xtlang
;; defining i so we can use it in dotimes.
;; Note that the value we set it to is unimportant.
($ (let ((i 0))
  (dotimes (i 2) (println i "iteration")))) ;; i is set to the default of 0
```

will print:
```xtlang
0 iteration
1 iteration
```

The value that we set `i` to in the `let` statement is unimportant. The code below will give us the same output:

```xtlang
;; Doesn't matter what we set i to.
($ (let ((i 1000000))
  (dotimes (i 2) (println i "iteration")))) ;; i is set to the default of 0
```

If we want to change the value that our counter starts on then we can do the following:

```xtlang
($ (let ((i 0)) ;; again the value here is unimportant
  (dotimes (i 2 1) (println i "iteration"))))
```

will print:

```xtlang
1 iteration
2 iteration
```

### doloop

Sometimes using a let statement in this way can be a little unwieldy and so `doloop` is provided as syntatic sugar for dotimes. Whenever you use `doloop` the compiler automatically creates the let-binding for your "counter" variable and creates a `dotimes` loop for you:

```xtlang
($ (doloop (i 2) (println i "iteration")))

;; this is identical to the doloop code above.
($ (let ((i 0))
  (dotimes (i 2) (println i "iteration"))))
```

__WARNING:__ While doloop is convenient, it can make your code inefficient:

```xtlang
;; inner loops
(doloop (i inum)
  (doloop (j jnum)
     (* i j)))

;; This generates the following
(let ((i 0))
   (dotimes (i inum)
       (let ((j 0))
          (dotimes (j jnum)
               (* i j))))

;; unfortunately the code above is extremely inefficient.

;; The same loop optimized
(let ((i 0)
      (j 0))
   (dotimes (i inum)
     (dotimes (j jnum)
       (* i j))))
```

The use of inner `let` statements in loop blocks is extremely inefficient. Consequently you should never nest `doloop`. Use it sparingly to clarify complex code.

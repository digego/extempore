---
title: Introduction to Functions
---

## Introduction

Like Scheme xtlang is a functional language. As you might expect, that functions are a key part of the language. In this chapter we'll look at how you define and call functions, before discussing how in xtlang all functions are essentially closures. If you don't know what a closure is, don't worry! By the end of this chapter you'll be an expert.

## Function Definition

In xtlang functions are defined using `lambda`:

```xtlang
(lambda (x)
  (+ x 1))
```

The function above takes a single variable `x` and adds `1` to it. The result of the expression ```(+ x 1)``` is returned by the function. Functions in xtlang _always_ return a value. The value returned is the last expression in the function:

```xtlang
;; A function that always returns 1
(lambda (x)
  1))
```

When you evaluated the code above you probably saw something like the following: ``#<<CLOSURE: 0x123543345>`` appear in your editor. This message telling you that you created a function. If want to be able to call this function then we need to **bind** it to a function name using `bind-func`:

```xtlang
(bind-func add1
  (lambda (x)
    (+ x 1)))
```

now we can call this function like so (remembering to use the `$` form as we're calling an xtlang function):

```xtlang
($ (add1 4)) ;; this returns 5
($ (add1 5)) ;; this returns 6
```

and we can call this function from other functions as well:

```xtlang
(bind-func advanced-math
  (lambda (y)
    (* y (add1 y))))
```

## Variable Assignment

Like Scheme you can define local variables using the `let` form:

```xtlang
(bind-func boring
  (lambda ()
    (let ((x 5)
          (y 4))
      (* x y))))

($ (boring)) ;; returns 20
```

You can also define local functions this way:

```xtlang
(bind-func adder
  (lambda (x)
    (let ((f (lambda (a)
                (+ a 1))))
      (f x))))

($ (adder 3)) ;; returns 4
```

Once you've created a variable you can change it's value using `set!`:

```xtlang
(bind-func silly-func
  (lambda (y)
    (let ((i 0))
      (set! i (+ y 1))
      i)))

($ (silly-func 4)) ;; returns 5
```

::: info
If you have a Scheme background it is worth noting that xtlang does not need the `let*` or `letrec` forms. Instead you can use `let` in all situations where you would need these in Scheme.
:::
::: info
## Scope

Local variables live only while they are in scope. As soon as they leave scope the variable can no longer be accessed.
:::

```xtlang
(bind-func scope-func
  (lambda (y)               ;; y is in scope
    (let ((i 0))            ;; i and y are in scope
      (let (v (+ i 1))      ;; v, i and y are in scope
        (set! i (+ v 1)))   ;; v, i and y are in scope
      (set! i (+ y v)))     ;; i and y are in scope.
                            ;;   You can no longer access `i`
    (+ y 1)))               ;; y is in scope.
                            ;;   You can no longer access `i` or `y`
```

The parameters passed to the function will remain in scope for the duration of the function call. Variables defined using a ```let``` form remain in scope only while the code inside the ```let``` that defined them is being executed. Once a variable has gone out of scope, the variable is destroyed and you can no longer access it.

::: info
xtlang varibles have lexical scope. If you don't know what this means, don't worry. It essentially means that scope behaves appropriately and you don't have to worry about weird side effects.
:::
::: info
## The Stack

A program when it is running has two blocks of memory: the stack, and the heap. The heap is used for long lived data. The stack in contrast is where short term information is placed. Every time you call a function the parameter values for that function are pushed on top of the stack, along with any book keeping data required by the function. When the function returns it frees (pops) this data and the stack unwinds to the point that it was before the function call.

[TODO Put a diagram of stack calls here]

Similarly whenever you allocate local variables in a let block, those variables are pushed onto the stack - when your code leaves the block that data is popped off the stack.
:::

```xtlang
(bind-func f
  (lambda (x)
    (+ (g x) x)))

(bind-func g
  (lambda (y)
    (+ y 2)))
```

So let's step through what happens when we call ```($ (f 4))```:

[TODO NEED a DIAGRAM showing progress through this.]

+ We enter the function ```f``` and push the value ```4``` onto the stack and associated with the variable `x`.
+ The function g is called with the value 4
+ 4 is pushed onto the stack and associated with the variable `y`.
+ ```g``` calculates the value for ```y + 2```
+ We leave the function ```g``` and the stack is popped, freeing `y` and returning the value `4`.
+ `f` now calculates the value for ```(+ 4 x)```, where `x` is `2`.
+ We now leave the function ```f``` and the stack is popped, freeing `x`, returning the value `6`.

## Recursion

A recursive function is a function that calls itself:

```xtlang
(bind-func adder
  (lambda (x)
    (if (<= x 0) 0
      (+ 1 (adder (- x 1))))))
```

Everytime `adder` runs it checks to see what `x` is. If `x <= 0` it returns `0`, otherwise it calls itself again with the value `x - 1`. So for example `(adder 3)` will result in the following:

+ `(adder 3)`
+ `(+ 1 (adder (- 3 1)))`
+ `(+ 1 (adder 2))`
+ `(+ 1 (+ 1 (adder (- 2 1))))`
+ `(+ 1 (+ 1 (adder 1)))`
+ `(+ 1 (+ 1 (+ 1 (adder (- 1 1)))))`
+ `(+ 1 (+ 1 (+ 1 (adder 0))))`
+ `(+ 1 (+ 1 (+ 1 0)))`
+ `(+ 1 (+ 1 1))`
+ `(+ 1 2)`
+ `3`

::: info
For more information on recursion and how to use it see the Scheme tutorial [When it's written...].
:::
::: info
Every time your function recurses it uses a little more stack space. While this is fine with small functions, with larger functions you can quickly run out of stack space. If your program uses up the stack then Extempore will crash:
:::

```xtlang
;; Don't run this!!
($ (println(adder 1000000)))
```

Fortunately it is possible to rewrite our function so it doesn't use any stack space.

### Tail Recursion

A function is tail recursive when the recursive call is the last thing executed by the function:

```xtlang
(bind-func boring
  (lambda (x)
    (if (<= x 0) 0
      (adder (- x 1)))))
```

So `(boring 3)` will result in this:

+ `(boring 3)`
+ `((boring (- 3 1)))`
+ `((boring 2))`
+ `(((boring (- 2 1))))`
+ `(((boring 1)))`
+ `((((boring (- 1 1)))))`
+ `((((boring 0))))`
+ `((((0))))`
+ `(((0)))`
+ `((0))`
+ `(0)`
+ `0`

In other words when a tail recursive function stops recursing there is no more work to do, and the stack unwinding is unnecessary. Consequently the compiler is able to take advantage of this and eliminate the need to store anything on the stack. Instead it just replaces the value of `x` with the new value each time it recurses:

+ `(boring 3)`
+ `(boring (- 3 1))`
+ `(boring 2)`
+ `(boring (- 2 1))`
+ `(boring 1)`
+ `(boring (- 1 1))`
+ `(boring 0)`
+ `0`

Resulting in extremely fast code that will never abuse your stack [fn:: The resulting code is equivalent to a C while loop]. So how would we take advantage of this for the `adder` function above. The trick to making that function tail recursive is to realize that we need to pass the answer in as a second parameter:

```xtlang
(bind-func adder-tail
  (lambda (x res)
    (if (<= x 0) res
      (adder-tail (- x 1) (+ res 1)))))

;; This works fine
($ (println(adder-tail 100000000 0)))

;; But this will crash:
($ (println(adder 1000000)))
```

This is a slightly ugly interface, so we wrap adder-tail in another function:

```xtlang
(bind-func better-adder
  (lambda (x)
    (let ((fn (lambda (x res)
              (if (<= x 0) res
                (fn (- x 1) (+ res 1))))))
      (fn x 0))))
```

And now we can run ```($ (println(better-adder 100000000)))``` with absolutely no problems.

## Closures {#closures}

Like with Scheme, all functions in xtlang are lexical closures. This means that any variable referenced in the scope of the function is captured along with the function, even if that variable was not passed in as an argument.

```xtlang
;; this function returns another function that has access to the original x parameter
(bind-func test-closure
  (lambda (x)
    (lambda (y) (+ x y))))

($ ((test-closure 4) 3)) ;; returns 7
```

The inner function has access to all the variables defined in the outer function, including the parameters that were passed into the function. We say that this function (which is anonymous) _closes over_ the variables in the outer function.

We can also do this:

```xtlang
(bind-func test-closure2
  (let ((x 4))
    (lambda (y)
      (* y x))))

($ (mul-math 4)) ;; returns 16
```

By using the `let` form before we define the function bound to `test-closure`, we create a closure that contains the variable x the function can access (or to use the correct terminology, it _closes over_ `x`).

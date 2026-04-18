---
title: The Basics
---

## Introduction

In this chapter we'll start you slowly by showing you some of the basics of programming in XTLang. We'll begin by showing you how to print things to the terminal, before introducing you to the basics of flow control.

## Printing to the terminal

Let's beging with everyone's favourite first program:

```xtlang
($ (println "Hello World!"))
```

If everything went according to plan you should see `Hello World!` appear on the terminal where you started Extempore. This code consists of two parts:

+ ```($ ...)```
+ ```(println "string goes here")```

The $ form is used to tell Extempore that the code you are sending it is XTLang code, rather than scheme. `println` as you might expect is used to tell xtlang to print your string to the terminal, followed by a carriage return.

`println` can do more than just print strings:

```xtlang
($ (println 4)) -- prints 4 to the command line.
($ (println 3 "+" 4 "=" (+ 3 4)))
```

The code above also introduces the new println function. println calls the polymorphic function print for each supplied argument, and ends with a carriage return. Note that in the example above XType and YType both have default print implementations. The result of calling (test1) is the log output…

XType:<5.000000,5> YType:<XType:<33.000000,3>,6>

println may also include additional string literals which will be printed in place …

;; println with string literals
(bind-func test3
  (lambda (x:i64 y:i64)
    (println "x sqr:" (* x x) " y cube:" (* y y y))))

(test2 2 2)

With output to the log for (test2 2 2) …

x sqr: 4  y cube: 8

Note that println automatically provides both spaces and a carriage return. You can avoid both the spaces and the carriage return by using printout, which in all other respects is identical. Note that only string literals will be interpreted correctly by println and printout. Attempting to print an i8* will result in a printed underscore - the case for any types without default print implementations. Extempore does now include a String type which does include a default print implementation. We strongly encourage the use of String in preference to i8* where possible.

;; string test
(bind-func test4
  (lambda ()
    (let ((s1 (String 5 "Hello"))
          (s2 (Str " World!"))
          (s3 (string_cat s1 s2)))
      (println s1 s2 "concatenated->>" s3))))

(test4)

With the output …

'Hello' ' World!' concatenated->> 'Hello World!'

It is worth noting that String provides additional constructors. The default String constructor requires both the size (in chars) of the string (without null terminator), as well as a c-string (i.e. i8*). The second constructor Str automatically derives the length of the c-string. Both are provided here for example. Str will be the go-to constructor for most day-to-day usage. Note that the String type is printed to the log with quotes - where string literals in the println are not.

You are strongly encouraged to start using String and co. in preference to i8* whereever possible. i8* should really only ever be used now for bridging to C. Additionally printf should now only be used as a last resort.





We can write some more code to bind-val a global variable myPI, which is an xtlang global variable of type double. If you evaluate this with Alt+S or C-M-x (or whatever the command is in your editor) then what happens is

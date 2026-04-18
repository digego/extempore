---
title: Strings
---

## Introduction

In this chapter we will introduce strings and the functions available to manipulate them.

Strings in xtlang are immutable. Whenever you call a function that manipulates a string it will return a new string leaving the original string unchanged:

```xtlang
($ (let ((str1 (String "hello world"))
         (str2 (substring str 0 5)))
        (begin
            (println str1)  ;; outputs 'hello world'
            (println str2)))) ;; outputs 'hello'
```

## The String Type

Confusingly xtlang has two string types so as to maintain compatibility with the 'C language'. These types are:

+ `i8*` - or an array of characters.
+ `String*` - an object for storing and manipulating strings.

Unless you're writing libraries that are interfacing directly with C code (this will be discussed in the chapter on C interopability), you should never be using `i8*` strings directly. However you will sometimes use them without realizing as the string literal type `"string"` has the type `i8*`:

+ `"hello":i8*`
+ `(String "hello"):String*`

Hopefully this state of affairs will be improved in the future, but for the moment we're stuck with this.

## Creating Strings

Strings are creating using the `String` function:

```xtlang
($ (String "My First String))

(bind-val my-string String "My String")

;; you can also create a new string from an old one. This creates a copy of the original string.
($ (String (String "hello")))
```

You can also use `Str` as an alias for `String` if you prefer.

Strings can be created from other types using the `toString function:

```xtlang
($ (toString 4))

(bind-val my-string-num2 String* (toString 4))
```

## String Length

To get the length of a string use length:

```xtlang
($ (length (String "hello"))) ;; 5
```

## String Comparisons

### Equality

To compare two strings use equal:

```xtlang
($ (equal (String "hello") (String "bob"))) ;; #f
($ (equal (String "hello") (String "hello"))) ;; #t
```

### Levenshtein Distance

The Levenstein Distance is the minimum number of single-character edits (insertions, deletions or substitutions) required to change one word into the other. xt_lang offers the `levenshtein` for this.

To get the length of a string use equal:

```xtlang
($ (levenshtein (String "hello") (String "hello"))) ;; 0
($ (levenshtein (String "hello") (String "hedo"))) ;; 2
```

## String Manipulation

XTlang gives you a number of functions for manipulating and accessing parts of strings.

### Trimming Whitespace

You can remove whitespace using the `trim`, `ltrim` and `rtrim` functions. `ltrim` removes white space at the beginning of a string, `rtrim` removes white space at the end of a string and `trim` removes whitespace at both ends of a string:


```xtlang
($ (ltrim (String "  hello world  "))) ;; 'hello world  '
($ (rtrim (String "  hello world  "))) ;; '  hello world'
($ (trim  (String "  hello world  "))) ;; 'hello world'
```

TODO Write `rtrim` and `ltrim`.... :)

### Substrings

You can create substrings from existing strings using substring:

```xtlang
;; Returns "rum"
($ (substring (String "Scrumptious") 2 5))
```

substring takes 3 parameters:

1. The original string.
2. start position for new substring
3. end position for new substring.

If the start, or end, positions lie outside the string then the function will return `null`.

NOTE: This may change in the future to something safer.

### Combining Strings

You can combine strings using `cat` and `cat2`:

```xtlang
;; returns "hello world"
($ (cat2 (String "hello") (String " world")))

;; returns "one".
($ (cat2 null (String "one")))

;; returns "hello my world"
($ (cat (String "hello ") (String "my ") (String "world")))
```

`cat2` takes two parameters, while `cat` generalizes to as many strings as you need to cobmine. If you try to combine a null with a string then `cat` and `cat2` will simply return the string.

### Replacing Substrings

The functions `replace` and `replace_all` can be used to replace parts of a string with a different string. `replace` replaces the first occurrence that it finds, and then returns the string. `replace_all` replaces every occurrence of the string.

```xtlang
;; returns (String "hello your face is my name")
($ (replace "hello your name is my name" "name" "face"))

;; returns (String "hello your face is my face")
($ (replace_all "hello your name is my name" "name" "face"))

;; returns (String "hello your name is my name")
($ (replace_all "hello your name is my name" "mouth" "face"))
```

`replace` and `replace_all` can be used with any combination of `String` and String literal that you like:

```xtlang
($ (replace (String "hello world") "hello" "goodbye"))

($ (replace "hello world" (String "hello") "goodbye"))
```

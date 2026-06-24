---
title: Core
description: Core xtlang functions and variables that load at launch.
---

The core functions and variables that load at launch.

# Memory

## size_t

Type: AliasAlias: i64 Value: Pointer Size (on 32 bit systems it is 32, on 64 bit
it will be 64 bit)

This should only be necessary if you're calling foreign functions.

## size_of_type

Type: Macro

### Parameters

1. type : type name

### Returns

- i64 : bytesize of this type.

### Description

Extempore's version of size_of (C/C++ function). How much memory is taken up by
a particular type. For tuples, and other types built upon memory references, the
size takes into account pointer sizes, and not the size of memory pointed to.

### Example

`($ (size_of i64))`

## inc

Type: Macro

### Parameters

1. `x` : Variable to be incremented. This must be a type that supports basic
   math functions.
2. `y` : Amount to increment `x`. This must the same type as `x`.

### Returns

- Returns the result. The return value will be the same type as `x`.

### Description

Increments x by y.

### Example

`($ (let ((x 5)) (inc x 1))) ;; returns 6`

## dec

Type: Macro

### Parameters

1. `x` : Variable to be decremented. This must be a type that supports basic
   math functions.
2. `y` : Amount to decrement `x`. This must the same type as `x`.

### Returns

- Returns the result. The return value will be the same type as `x`.

### Description

Decrements x by y.

### Example

`($ (let ((x 5)) (dec x 1))) ;; returns 4`

## min

Type: Macro

### Parameters

1. `x` : Variable to be compared. This must be a type that supports less
   than/equal.
2. `y` : Variable to be compared. This must the same type as `x`.

### Returns

- Returns the smallest value. The return value will be the same type as `x`.

### Description

Minimum of `x` and `y`.

### Example

`($ (min 3 7)) ;; returns 3`

## max

Type: Macro

### Parameters

1. `x` : Variable to be compared. This must be a type that supports less
   than/equal.
2. `y` : Variable to be compared. This must the same type as `x`.

### Returns

- Returns the largest value. The return value will be the same type as `x`.

### Description

Returns the largest of the two values.

### Example

`($ (max 3 7)) ;; returns 7`

## clamp

Type: Macro

### Parameters

1. `x` : Input value. Must be a type that supports <>=.
2. `minval` : Minimum value in range. Must be same type as `x`.
3. `maxval` : Maximum value in range. Must be same type as `x`.

### Returns

- The return value is the same type as `x`.

### Description

Clamp input value to range. If `x < minval` it will return minval, if
`x > maxval` it will return maxval. Otherwise it returns x.

### Example

`($ (clamp 12 0 10)) ;; returns 10`

## log

Type: Closure

### Types

- log:[float,float]
- log:[double,double]

### Description

returns the natural logarithm (ln)

## logn

Type: Macro

### Types

- `float`
- `double`

### Parameters

1. `x` : x value
2. `base` : base of the logarithm

### Description

Returns log of x with base `base`

### Example

`($ (log 100, 10)) ;; log10 of 100 (returns 2)`

## deg2rad

Type: Macro

### Supported Types

- `float`
- `double`

### Parameters

1. `degrees` : x value in degrees

### Returns

- Same type as `degrees`

### Description

Converts a value in degrees to radians.

### Example

`($ (deg2rad 180.0)) ;; returns 3.141593 (pi)`

## rad2deg

Type: Macro

### Supported Types

- `float`
- `double`

### Parameters

1. `radians` : x value in radians

### Returns

- Same type as `radians`

### Description

Converts a value in radians to degrees.

### Example

`($ (rad2deg PI)) ;; returns 180.0`

## evenp

Type: Macro

### Supported Types

- i8
- i16
- i32

### Parameters

1. x - the value that you are testing. Must be an integer of some kind.

### Return Value Type

- `i1` : 1 if `x` is even, 0 otherwise

### Description

Checks to see if a value is even. Returns a C boolean (i1).

## oddp

Type: Macro

### Supported Types

- i8
- i16
- i32

### Parameters

1. x - the value that you are testing. Must be an integer of some kind.

### Return Value Type

- `i1` : 1 if `x` is odd, 0 otherwise

### Description

Checks to see if a value is even. Returns a C boolean (i1).

# Math Constants

## NaNf

Constant Type: float Value: Single precision not-a-number

## NaN

Constant Type: double Value: Double precision not a number

## PIf

Constant Type: float Value: pi (3.14159...)

## TWOPIf

Constant Type: float Value: 2 \* pi (6.2831...)

## Ef

Constant Type: float Value: e (base of natural log)

## PI

Constant Type: double Value: pi (3.14159...)

## TWOPI

Constant Type: double Value: 2 \* pi (6.2831...)

## E

Constant Type: double Value: e (base of natural log)

# Math Functions

## random

Type: Polymorphic Closure

### Types

- [float]\*
- [double]\*
- [i32,i32]\*
- [i64,i64]\*
- [float,float]\*
- [double,double]\*
- [i32,i32,i32]\*
- [i64,i64,i64]\*
- [float,float,float]\*
- [double,double,double]\*

### Description

If called with 0 arguments it will return a pseudo random value between 0.0 and
1.0

If called with 1 argument, `arg1` it will return a pseudo random value `x` where
0 <= x < arg1.

If called with 2 arguments, `arg1` and `arg2` it will return pseudo random
values `x` where arg1 <= x < arg2.

## tan

Type: Polymorphic Closure

### Types

- [float,float]\*
- [double,double]\*

### Parameters

1. `x` : Angle in radians.

### Description

Returns tan(x)

## atan

Type: Polymorphic Closure

### Types

- [float,float]\*
- [double,double]\*

### Parameters

1. `x`

### Returns

- Angle of x in radians.

### Description

Inverse tangent. Returns atan(x)

## tanh

Type: Polymorphic Closure

### Types

- [float,float]\*
- [double,double]\*

### Parameter

1. `x` : Angle in radians

### Description

Hyperbolic tangent. It returns tanh(x)

## sin

Type: Polymorphic Closure

### Types

- [float,float]\*
- [double,double]\*

### Parameters

1. `x` : Angle in radians.

### Description

Returns sin(x)

## asin

Type: Polymorphic Closure

### Types

- [float,float]\*
- [double,double]\*

### Parameters

1. `x`

### Returns

- Angle of x in radians.

### Description

Inverse sine. Returns atan(x)

## sinh

Type: Polymorphic Closure

### Types

- [float,float]\*
- [double,double]\*

### Parameter

1. `x` : Angle in radians

### Description

Hyperbolic sine. It returns sinh(x)

## cos

Type: Polymorphic Closure

### Types

- [float,float]\*
- [double,double]\*

### Parameters

1. `x` : Angle in radians.

### Description

Returns cosine(x)

## acos

Type: Polymorphic Closure

### Types

- [float,float]\*
- [double,double]\*

### Parameters

1. `x`

### Returns

- Angle of x in radians.

### Description

Inverse cosine. Returns acos(x)

## cosh

Type: Polymorphic Closure

### Types

- [float,float]\*
- [double,double]\*

### Parameter

1. `x` : Angle in radians

### Description

Hyperbolic cosine. It returns cosh(x)

# Audio Constants and Types

## SAMPLE

Type Alias Type: float Value Audio I/O sample type in the dsp processing chain

## DSP

**Type Alias** : [SAMPLE,SAMPLE,i64,i64,SAMPLE\*]\*

### Parameters

1. **in:SAMPLE** : input sample from the sound card
2. **time:i64** : the current time, in samples (the audio sample clock)
3. **chan:i64** : the channel that the current sample comes from, and the
   corresponding channel that the current sample will go to.
4. **dat:SAMPLE\*** : pointer to user data passed through to the callback.

### Returns

- **SAMPLE** : output sample. This will be sent to chan

### Description

Audio callback function that is single sample and single threaded. A dsp closure
has the signature `(lambda (in time chan dat) ...)`.

### Example

```extempore
(bind-func dsp:DSP
  (lambda (in time chan dat)
    (* 0.1 (random))))
```

## DSPMT

**Type Alias** : [SAMPLE,SAMPLE\*,i64,i64,SAMPLE\*]\*

### Parameters

1. **in:SAMPLE\*** : pointer to the block of input samples (one per input
   channel).
2. **time:i64** : the current time, in samples (the audio sample clock)
3. **chan:i64** : the channel that the current sample comes from, and the
   corresponding channel that the current sample will go to.
4. **dat:SAMPLE\*** : pointer to user data passed through to the callback.

### Returns

- **SAMPLE** : output sample. This will be sent to channel 'chan'

### Description

The multithreaded variant of [DSP](#dsp): the `in` argument is a pointer to the
block of input samples rather than a single sample.

### Example

```extempore
(bind-func dsp:DSPMT
  (lambda (in time chan dat)
    (* 0.1 (random))))
```

## SPI

Constant Type: SAMPLE Value: PI (3.1415...)

## STWOPI

Constant Type: SAMPLE Value: 2 \* PI (6.2831...)

## SE

Constant Type: SAMPLE Value: e (base of natural log)

## SAMPLE_RATE

Constant Type: i32 Value: Audio sound card sample rate

## SAMPLERATE

Constant Type: SAMPLE Value: Audio sound card sample rate

## SRs

Constant Type: SAMPLE Value: Audio sound card sample rate.

## SRf

Constant Type: float Value: Audio sound card sample rate

## SRd

Constant Type: double Value: Audio sound card sample rate

## SR

Constant Type: i64 Value: Audio sound card sample rate

## CHANNELS

Constant Type: i32 Value: Number of audio output channels on sound card.

## IN_CHANNELS

Constant Type: i32 Value: Number of audio input channels on sound card.

## NUM_FRAMES

Constant Type: i32 Value: Audio signal block size

## FRAMES

Constant Type: i64 Value: Audio signal block size

# I/O

## print_return

`print_return::[void]*`

### Description

Prints a line return to the output buffer (e.g. where extempore is running).

## print_space

`print_space::[void]*`

### Description

Prints a space to the output buffer (e.g. where extempore is running).

## print

`print` type: overloaded

### Types

- [void,i1]\*
- [void,i8]\*
- [void,i16]\*
- [void,i32]\*
- [void,i64]\*
- [void,float]\*
- [void,double]\*
- [void,!a]\*
- [void,mzone*]\*
- [void,Symbol*]\*

### Description

Prints the value to the terminal.

For generic types, the generic type must be one of the following:

- An array containing a type that can be printed
- A pointer to a type that can be printed
- A tuple of length 1->6 containing types that can be printed.
- A user defined type that provides a `print` function.
- A memory zone
- A symbol (empty symbols will print a zero length symbol)

Overloads and should be used via the `println` function If the symbol is empty
will print a zero length symbol

### Description

Prints a line return to the output buffer (e.g. where extempore is running).

# Memory

## Zone

TODO Type?

### Parameters

1. `size:i64` : Size in bytes TODO Is this correct?

### Returns

- TODO?

### Description

TODO

## push_zone

TODO Need a type

### Parameters

1. TODO

### Returns

- TODO?

### Description

TODO

## pop_zone

TODO Need a type

### Parameters

1. TODO

### Returns

- TODO?

### Description

TODO

## reset_zone

TODO Need a type

### Parameters

1. TODO

### Returns

- TODO?

### Description

TODO What does this do?

## destroy_zone

TODO Need a type

### Parameters

1. TODO

### Returns

- TODO?

### Description

Frees the memory in the zone.

## peek_zone

TODO Need a type

### Parameters

1. TODO

### Returns

- TODO?

### Description

TODO

## zcopy

Type: Polymorphic Closure

### Parameters

1. `x` : size of memory area to be copied (the units are the size of the type.
   e.g. if the memzones are in i64s, and x is 4, then it will be 4 \*
   sizeof(i64)
2. `fromz` : a pointer to the zone to be copied
3. `toz` : a pointer to the destination memory zone

### Types

This function should support all built in types and user defined types.

### Description

A function to copy data from one memory zone to another.

# Control Flow

## ->

Type: Macro

### Description

A 'thread-first' macro.

This macro can help make code more readable by removing nesting. Threads the
first expr passed to the macro into the first position in the second sexp.
Recursively continues to thread the resultant sexp into any further sexp
arguments.

### Examples

`($ (-> 5 (+ 3) (* 2))) ;; (* (+ 5 3) 2), returns 16`

## ->>

Type: Macro

### Description

A 'thread-last' macro.

This macro can help make code more readable by removing nesting. Threads the
first expr passed to the macro into the last position in the second sexp.
Recursively continues to thread the resultant sexp into any further sexp
arguments.

### Examples

`($ (->> 5 (- 10))) ;; (- 10 5), returns 5`

# Interop

## get_native_name

type: scheme macro

## Parameters

1. `closure-name` : Extempore closure name

## Returns

- A string that can be used in Scheme to call this closure.

## Description

See Extempore-Scheme interop description in the tutorial.

## get_native_fptr

type: macro

### Parameters

1. `closure-name` : Extempore closure name

### Returns

- A a pointer to the function that can be used by other languages (e.g. C) for
  callbacks, etc.

### Description

See FFI description in the tutorial.

---
title: Pattern language
---

Before we start, make sure you've read at least the [quickstart
guide](../overview/quickstart.md) and you can set
up & evaluate Extempore code on your machine.

## Introduction

The Extempore pattern language is a
[DSL](https://en.wikipedia.org/wiki/Domain-specific_language) for specifying
repeating patterns and musical transformations (beats & loops).

The pattern language is not actually tied to music-making; it's really just a
convenient scaffold for writing code which will (repeatedly) be executed with
specific timings and arguments. However, making music is a pretty significant
motivator for the whole thing, so there are lots of helper functions geared
towards doing just that.

### Setting up an audio signal chain

First, let's load up the necessary audio & pattern language infrastructure:

```extempore
(sys:load "examples/sharedsystem/audiosetup.xtm")
```

Depending on your machine it might take a little while, but hang tight---you'll
get there in the end. When you see something like this then you're ready to go.

```
Compiled:  active_notes >>> [i32,i8*]*
Compiled:  dsp_load >>> [void]*
Compiled:  main_reverb >>> [void,i64,float]*
Compiled:  main_gain >>> [float,float]*
sharedsystem audio setup complete
```

You've just loaded

- three analogue synths (`syn1`, `syn2` and `syn3`)
- a synth drum kit (`kit`)
- a piano sampler (`samp1`)

Don't worry about how to use them just yet, you'll see how in a minute.

::: info
Extempore's audio signal chain is highly flexible, so you can set up your
noisemaking infrastructure in a way which suits you. However, if you're new to
all this and just want to load up a few synths and samplers, then the best way
to do this is to load the Extempore sharedsystem---including the analogue synth
& built-in samplers as covered in [this guide](analogue-synth.md).

These pattern language and the sharedsystem are conceptually independent---you
certainly don't have to understand the pattern language in depth if you just
want to make weird & awesome noises on the synths (or vice versa).

However, even if you're mostly interested in the pattern language it's still
handy to understand a bit about the analogue synth for e.g. using loops to
change synth parameters with musically meaningful timings. That's the approach
we'll take in this guide---we won't necessarily explain the sharedsystem stuff,
but you can always jump over to e.g. the [analogue synth
guide](analogue-synth.md) to go deeper.

Finally, if you _don't_ load the sharedsystem, then make sure you at least load:

```
(sys:load "libs/core/pattern-language.xtm")
```

because that's the actual file where the important functions and macros
associated with the pattern language live.
:::

## Pattern basics

A pattern looks like this:

```extempore
(:> pat-1 2 0 (play syn1 @1 80 dur) (list 60 58 60 63))
```

The parts of this pattern are:

- the `:>` macro, which tells Extempore that the rest of this expression is a
  pattern

- the name of the pattern (in this case `pat-1` but any valid scheme variable
  name is ok)

- the total length in beats of the pattern (in this case `2`)

- the offset in beats of the pattern (in this case `0`)

- the "pattern expression" (in this case `(play syn1 @1 80 dur)`) which is the
  expression which is evaluated at each "triggering" of the pattern

- one (or more) "pattern lists" (in this case ``(list 60 58 60 63)``); these are
  lists of values which the pattern will loop through

If you eval the above pattern in Extempore, you'll hear a repeated synth line.
You can modify & re-evaluate it and hear it change---try changing one of the
numbers in the pattern list and see what happens.

Remember, any change won't take effect until you re-evaluate the expression. But
it gets boring if we remind you _every time_, so if they change you're trying to
make isn't working remember to check that you've evaluated it.

Just a heads up: the specific pattern examples given in this guide will probably
get pretty boring, looping hundreds/thousands of times as you read through this
content. Feel free to change any of the details (e.g. the lists of pitch values)
as you go---the more you play around with things, the deeper your understanding
will be.

For now you don't have to understand exactly what every part of the pattern
expression `(play syn1 @1 80 dur)` means (in short, the arguments represent
_instrument_, _pitch_, _velocity_ and _duration_; there are [other
guides](note-level-music.md) which
explain it in much detail). The main thing to know is that each time the pattern
expression is triggered the `@1` will be replaced by successive values from the
pattern list. First `60`, then `58`, then `60`, then `63`, then back to the
beginning---in fact it will keep cycling through that list forever.

One more note about timing: there's a global metronome in Extempore (which
defaults to 120bpm at startup). But you can change it at any time (changing the
global tempo will affect the playback rate of _every_ pattern). To set the tempo
to 72bpm:

```extempore
(*metro* 'set-tempo 72)
```

Soon, you'll wonder how you stop the pattern. The answer is that you change the
`:>` macro into a `:|`. As long as the name is the same as the one you gave it
when you _started_ the pattern (e.g. `pat-1`) you can leave the rest of the
expression the same, so when you evaluate this:

```extempore
(:| pat-1 2 0 (play syn1 @1 80 dur) (list 60 58 60 63))
```

you'll hear blessed silence 😉 This small `:>` -> `:|` change means that it's
easy to stop a pattern and re-start it again; just change back to `:>` and
re-eval the code.

::: info
If you're familiar with MIDI note numbers those numbers are probably pretty
familiar (60 for middle C, etc). If you're more familiar with 12-tone note
names, [a little later on in this
guide](#using-note-names-instead-of-midi-note-numbers) you'll see that you can
use symbols like `bb3`, `b`, `c4`, `c#4`, `d4` instead. But for now let's stick
with the note numbers and get our head around the timing stuff.
:::

### How does the timing work?

Try re-starting the loop (note the `:>`) and changing the values in the list at
the end, e.g.

```extempore
(:> pat-1 2 0 (play syn1 @1 80 dur) (list 60 58 60 63 60 61))
```

Notice that the duration of the individual notes gets shorter, so that the
overall loop takes the same length of time. Previously, the loop length was 2
beats and there were 4 values in the list, so each note was 2 divided by 4 = 1/2
a beat long (i.e. a quaver or eighth note). Now there are six numbers in the
list, so that's 6 notes over 2 beats. The list goes through a full "loop" in the
same amount of time, so each note must be shorter.

To achieve the opposite---notes are the same length as before, but the total
length of the loop is longer---we need to update the total length argument (the
one after the pattern name) as well:

```extempore
(:> pat-1 3 0 (play syn1 @1 80 dur) (list 60 58 60 63 60 61))
```

Here are two different patterns with different loop durations. They're both
still playing their notes on the same instrument (`syn1`) but the list of pitch
values are different.

```extempore
(:> pat-1 2 0 (play syn1 @1 80 dur) (list 60 58 60 63 60 61))
(:> pat-2 4 0 (play syn1 @1 80 dur) (list 67 67 67 48 36 65))
```

When there are more than one pattern playing simultaneously we can see the
effect of modifying the offset parameter. Compare

```extempore
(:> pat-1 2 0 (play syn1 @1 80 dur) (list 60 58 60 63 60 61))
(:> pat-2 4 0 (play syn1 @1 80 dur) (list 67 67 67 48 36 65))
```

with (hint: the only change is to `pat-2`)

```extempore
(:> pat-1 2 0 (play syn1 @1 80 dur) (list 60 58 60 63 60 61))
(:> pat-2 4 1 (play syn1 @1 80 dur) (list 67 67 67 48 36 65))
```

There are a couple of special symbols in the which are helpful in understanding
how the timing works (i.e. when the pattern expression is actually called to
play the note).

If an element of the list is the underscore symbol (`_`) then the pattern will
"skip" that execution (in musical terms, it's a _rest_). Try replacing one (or
more) of the numeric values in the pattern list, e.g.

```extempore
(:> pat-2 4 1 (play syn1 @1 80 dur) (list 67 67 67 '_ 63 65))
```

Note that the underscore actually has a tick/apostrophe `'` before it. This is
called "quoting" the expression, and it's just a way of telling Extempore that
the `_` character isn't the name of a variable (which should be evaluated to a
value). Try running the above pattern without the `'` and you'll get an error:

```
pattern starting  pat-2
eval: unbound variable: _
Trace: pat-2
```

There's some subtlety here (especially because even though `_` is a weird name
for a variable compared to say `x`, `y` or `pitch`, it _is_ a valid symbol name
in Scheme). But the main point with _quoting_ the `_` is to tell Scheme to just
treat it like a symbol (kindof like a name) rather than to evaluate it to get
the symbol's value.

This is relevant because the quote `'` operator can actually apply to a whole
list (everything inside a balanced pair of parens `(` `)`) like so:

```extempore
(:> pat-2 4 1 (play syn1 @1 80 dur) '(67 67 67 _ 63 65))
```

Note that the `list` isn't required at the start anymore---everything inside the
brackets (i.e. `67`, `67`, `67`, `_`, `63` and `65`) is just treated as the
elements of the list (no further evaluation is done). Don't stress about the
details too much right now, but just remember that you have to quote the `_` and
any other symbols in the pattern list (unlike numbers, which are special and
don't need to be quoted). We'll use that quote symbol a fair bit from here in
this guide.

If an element of the list is the pipe/vertical bar symbol (`|`) then the pattern
will also "skip" that execution, but the duration of that slot in the pattern
list will be added to the _previous_ value (in musical terms, it's a _tie_). Try
replacing one (or more) of the numeric values in the pattern list, e.g.

```extempore
(:> pat-2 4 1 (play syn1 @1 80 dur) '(67 67 67 | 63 65))
```

these can even "stack", just like musical ties

```extempore
(:> pat-2 4 1 (play syn1 @1 80 dur) '(67 67 67 | | 65))
```

### Sublists for sub-dividing the beats

These patterns aren't very rhythmically interesting; you might be wondering how
you move beyond these plodding equal-duration loops.
The pattern language allows you to sub-divide the beats using sub-lists in the
pattern list.

Assuming that `pat-1` is running as before, change `pat-2` to:

```extempore
(:> pat-2 4 1 (play syn1 @1 80 dur) '(67 67 67 48 36 (61 65)))
```

Note that the final two notes are "half" duration, because they're in a sublist.
This can go on recursively:

```extempore
(:> pat-3 4 0 (play syn1 @1 80 dur) '(48 (46 (49 46))))
```

The lists (and sub-lists) in the pattern language aren't required to have nice round (or
even) numbers of elements: you can have triplets.

```extempore
(:> pat-3 4 0 (play syn1 @1 80 dur) '(48 (54 _ 46)))
```

Or even lists and sublists of length 7, or 15, or 115. Go wild. It also means
that there are multiple ways of specifying any one sequence of pitches &
durations---these two will sound identical:

```extempore
(:> option-1 4 0 (play syn1 @1 80 dur) '(60 | 48 61))
(:> option-2 4 0 (play syn1 @1 80 dur) '(60 (48 61)))
```

Which one you prefer is up to you. My advice; don't agonise over optimality in the pattern
stuff, just make some noise which sounds good 😉

### Playing multiple notes at once

Finally, you probably want to play multiple notes simultaneously---harmony's
pretty cool, after all. You already know one way to do this: just have multiple
patterns with the same (or even different) lengths and run them simultaneously.

```extempore
(:> chord-l 4 0 (play syn1 @1 80 dur) '(60))
(:> chord-m 4 0 (play syn1 @1 80 dur) '(63))
(:> chord-h 4 0 (play syn1 @1 80 dur) '(67))
```

However, since this is such a common thing (from a musical perspective) the
pattern language has one more trick up it's sleeve: using vectors (instead of
lists) to specify events/values which are to be triggered simultaneously. Here's
the same C-minor chord from the previous example:

```extempore
(:> chord-all 4 0 (play syn1 @1 80 dur) '(#(60 63 67)))
```

::: info
Again, Scheme---the programming language that we're using here---considers
[lists](https://www.scheme.com/tspl4/objects.html#./objects:h3) and
[vectors](https://www.scheme.com/tspl4/objects.html#./objects:h9) to be
different types of collections. However, if you don't care about the subtleties
and just want to make bangers remember that lists will either look like e.g.
this `(list 1 2 3)` or this `'(1 2 3)` (or occasionally this `` `(1 2 3)``),
while vectors look like this `(vector 1 2 3)` or this `#(1 2 3)`.

Again, that one "minor chord" vector counts as just one element in the pattern
list for duration purposes. In that example the `chord-all` pattern just has one
value in the pattern list (the vector `#(60 63 67)`), and since it's a 4-beat
pattern then the chord will play for 4 beats before re-triggering.

Like with all this stuff, you can in general combine the different features of
the pattern language together to play classic vi-IV-I-V pop anthems (including a
nice suspended 4th---and resolution---on chord V).
:::

```extempore
(:> chord-progression 16 0 (play syn1 @1 80 dur)
	'(#(60 63 67) #(60 63 68) #(58 63 67) (#(58 63 65) #(58 62 65))))
```

Or, y'know, do other stuff. Extempore's not judgemental.

### Multiple pattern lists

Sometimes you want to have more than one value in your pattern expression vary
over time, and the `:>` pattern macro allows _multiple_ pattern lists for this
purpose. Let's go back to the original example:

```extempore
(:> pat-1 2 0 (play syn1 @1 80 dur) '(60 58 60 63))
```

If we want to add accents to the third (`60`) note, we could add another list of
velocities (loudnesses) for the pattern language would loop through.

```extempore
(:> pat-1 2 0 (play syn1 @1 @2 dur) '(60 58 60 63) '(70 70 100 70))
```

Note that there's nothing which says that this second list has to be a list of
velocities, just like there's nothing which says that this first list has to be
a list of pitches. Each time through the pattern the current value from the
_first_ pattern list replaces the `@1` in the pattern expression, the current
value from the _second_ pattern list replaces the `@2` in the pattern
expression, and so on.

So we can switch the pattern lists around as long as we switch the `@1` and the
`@2` around---this will be exactly the same as before (so it's not a very
interesting change to make).

```extempore
(:> pat-1 2 0 (play syn1 @2 @1 dur) '(70 70 100 70) '(60 58 60 63))
```

One caveat with this multiple lists stuff: the note duration is always based on
the length of first list (as described above). If a second (or third...) pattern
list is shorter than the first one the values will be recycled, but the pattern
list position will still be reset to the beginning once the first pattern
completes. If the other pattern list is longer than the first one, the "extra"
values at the end won't be used. This behaviour can be used to your advantage,
allowing you to have interesting 4-against-3 or 17-against-6 interactions
between the values of your lists. Play around and have fun!

## What can I put in the pattern expression?

So far we've seen a few different examples of how to control what values get
"looped" through our pattern expression (which was `(play syn1 @1 80 dur)`
pretty much the whole time). Why do we even bother putting that in there if it's
not going to change? Well, there are sometimes good reasons to mix it up with
our pattern expression.

The key concept here is that the pattern expression can be arbitrary Scheme
code, so you can do _anything_ in there. Sure, a lot of the time you'll just
play through the "standard" pattern lists of pitches, velocities and maybe
durations. But sometimes you need more flexibility than that, and you've got the
power of a whole programming language to do it.

### Special pattern expression variables {#special-pattern-expression-variables}

Consider the examples we've been looking at all along, e.g.

```extempore
(:> pat-1 2 0 (play syn1 @1 80 dur) '(60 58 60 63))
```

Notice the final `dur` argument to the `play` function. If we do something like
this, you can hear the result (unless your `syn1` patch has a long release
time):

```extempore
(:> pat-1 2 0 (play syn1 @1 80 (* dur 0.5)) '(60 58 60 63))
```

The value of the `dur` argument will be the current length (in beats, as always)
of the current "note". It's bound (i.e. has a value) only inside the pattern
expression. It's there because it's really convenient to know how long the
current note is.

Remember that there's no reason that your pattern expression _has_ to trigger a
musical note; it could print to the log or just evaluate to a number (although
this "return value" doesn't go anywhere, so that's not so useful). Still, it's
often convenient to refer to it as the "note expression", "note duration", etc.
so in this guide sometimes we'll use that language. Just remember that your
expression doesn't have to be triggering a musical note in the traditional
sense.

::: info
We can use this to our advantage for debugging: if you replace the `(play ...)`
pattern expression with something like `(println @1)` you can see what the value
of `@1` (i.e. the component of the first pattern list) is printed to the log
each time through the list.

There are a couple of other variables which are bound inside the pattern
expression:

- `beat` is a number which represents the number of beats (since the start of
  your extempore session)
- `dur` is the length of the current note
- `LC` is another number which represents the _loop count_ (while `beat` goes up
  every beat---including half & quarter beats, etc.---`LC` only goes up by whole
  numbers, and only once a full loop through the first pattern list is
  completed)
- `LL` is just the _loop length_ in beats (i.e. it's the value after the name of
  the loop in the `:>` expression)
- `LP` is the _loop position_ in beats (the difference between `beat` and `LP`
  is that beat keeps counting up forever, but `LP` resets to zero each time
  through the loop)

All of these variables can be super-useful for making interesting musical
patterns. For example, say you want to (for a particular part of the pattern)
alternate between two notes each time through the pattern. You could do a modulo
arithmetic checks on the `LC` (loop count) variable like so:
:::

```extempore
(:> pat-1 2 0 (play syn1 @1 80 dur) (list 60 58 60 (if (= (% LC 2) 0) 66 63)))
```

Anyway, that's cool, but it's such a common thing to want to do that there's a
function called `orbit` (the shortened version `orb` also works) which does the
same thing, so the previous pattern is equivalent to

```extempore
(:> pat-1 2 0 (play syn1 @1 80 dur) (list 60 58 60 (orb LC 2 66 63)))
```

::: info
Note that we've gone back to using `(list ...)` rather than the quote operator
`'`. This is because with an expression like `(list 1 2 (orb LC 2 66))` you
_don't_ want the `orb` function to just be "quoted" as-is, you want it to be
evaluated (so that the pitch orbit actually happens). Sometimes it'll be more
convenient to use `(list ...)`, and sometimes to quote things `'(... )`. You can
[do whole courses on this
stuff](https://courses.cs.washington.edu/courses/cse341/04wi/lectures/14-scheme-quote.html).

You don't have to do it every _second_ time through the loop, either; you can do
it every 3rd (or 4th, or 5th...)
:::

```extempore
(:> pat-1 2 0 (play syn1 @1 80 dur) (list 60 58 60 (orb LC 3 66 63)))
```

The orbit function has one more nice property; if you leave off the final
argument (i.e. the note to play when it's _not_ the 2nd/3rd/4th time through the
pattern or whatever) then it'll return a `_` behind the scenes, so your pattern
won't play anything.

```extempore
(:> pat-1 2 0 (play syn1 @1 80 dur) (list 60 58 60 (orb LC 3 66)))
```

`orb` is great when you want to alternate a subset of your pattern list; what
about when you want to cycle through two different pattern lists? In some cases
you could append (join) the lists together and just double the loop length and
you'd get the desired effect. But what about if you want to go 3 times through
the first pattern list, then 3 times through a second one? Your list is going to
get pretty long and unweildy.

That's where the `cycle` function comes in. Again, it uses the `LC` loop count
variable to "slow down" the rate of going through multiple pattern lists. Here's
an example:

```extempore
(:> pat-1 2 0 (play syn1 @1 80 dur) (list 60 58 60 (cycle LC 1 '(72 67) '(73 72))))
```

Note how the pattern alternates between `(72 67)` and `(73 72)` for the final
two notes of each pattern? Also, remember that because these are sublists of the
bigger pattern they're only half the duration of the other notes in the pattern
(60, 58 and 60).

If you want to not alternate, but do 2 of one then 2 of the other, then change
the second argument of the `cycle` function:

```extempore
(:> pat-1 2 0 (play syn1 @1 80 dur) (list 60 58 60 (cycle LC 2 '(72 67) '(73 72))))
```

Through all this, you're putting tools in your tool-belt for making interesting
musical patterns.

### Using note names instead of midi note numbers {#using-note-names-instead-of-midi-note-numbers}

As mentioned [earlier in this guide](#midi-note-number-teaser-box), in your
pattern expression you can use symbols like `c4` (which evaluates to 60; it's
middle C) to specify notes (so-called [scientific pitch
notation](https://en.wikipedia.org/wiki/Scientific_pitch_notation)). These
special pitch variables are all 2 or 3 characters long:

1. the first character is the note letter (pitch class), e.g. `a`--`g`
2. the middle (optional) character is the accidental modifier, either `#` for
   sharps and `b` for flats
3. the final character is a number representing the octave, with `4` being the
   octave of middle C

A couple of examples:

- `c#4` is one semitone above middle C
- `a4` is the A above middle C ([A440](https://en.wikipedia.org/wiki/A440_(pitch_standard)))
- `bb5` is a Bb nearly 2 octaves above middle C
- `c2` is a C two octaves below middle C

The only real "gotcha" is that the index number goes up in octaves that are
"C-based", so e.g. `a3` is pitched _above_ `c3`. But if you're used to thinking
with this stuff you're probably used to that anyway.

Again, these are special variables which are only bound in the context of a
pattern expression (just like `beat`, `dur`, `LC` etc). But if you want to put
them in your pattern lists then they can be really handy. One more quick example:

```extempore
(:> cello-suites 4 0 (play syn1 @1 80 dur) '(c3 e3 g3 c4 e4 g3 c4 e4))
```

Using our `cycle` function from earlier we can even go further:

```extempore
(:> cello-suites 4 0 (play syn1 @1 80 dur)
    (cycle LC 2
           '(c3 e3 g3 c4 e4 g3 c4 e4)
           '(c3 d3 a3 d4 f4 a3 d4 f4)))
```

Also, this is a reminder that there's no reason you have to have your whole
pattern on one line in your text editor---split it up if it makes it easier for
you (or your audience) to see & work with.

### Scales, roots, chords

The previous pattern has (hopefully) whet your appetite for working with scales,
notes, chords, melody, harmony, and all those things[^eurocentric].

[^eurocentric]:
    The eurocentric nature of all these music theory helper functions and
    variables isn't lost on me; if you'd like to discuss how to build a set of
    post-colonial extensions to the pattern language then [get in
    touch](mailto:ben.swift@anu.edu.au).

To generate a pattern list which plays a scale, there's a `scale` function. For
example, to play a scale which starts in the 3rd octave and goes for 8 notes, use:

```extempore
(:> asc-scale 2 0 (play syn1 @1 80 dur) (scale 3 8))
```

You'll hear an ascending natural minor (i.e. the [aeolian
mode](https://en.wikipedia.org/wiki/Aeolian_mode)) scale starting on `c3`.

You can see the pattern list directly by evaluating something like this
_outside_ a pattern expression

```extempore
(println (scale 3 8))
```

which will print `(48 50 51 53 55 56 58 60)` to the log view---these are the
midi pitch numbers of the minor scale starting one octave below middle C. In
other words, the `scale` function returns a list which can be directly used as
the pattern list, and (as always) that pattern will loop forever.

If you're wondering why it plays that specific scale---and how you could play a
_different_ scale---then the reason is that there are two more "special"
variables, `*root*` and `*scale*`, which are [initialised to the following
values at
start-up](https://github.com/extemporelang/extempore/blob/master/libs/core/pattern-language.xtm):

```extempore
(define *root* 0)
(define *chord* '(36 60 63 67))
(define *scale* (pc:scale 0 'aeolian)) ;; 0 for the C pitch class (C# would be 1, etc.)
```

::: info
These are almost like the [previous special
variables](#special-pattern-expression-variables) (`dur`, `LC` etc.) with the
exception that these ones _do_ exist (and can be modified) outside a pattern
expression. This is because while `dur` (potentially) changes every time through
the pattern list, things like chords, roots and scales _usually_ are held
consistent on slightly larger timescales (bars, etc.) and so we define them
outside an individual pattern expression, and can also change them outside the
expression with `set!`.

By default, `scale` (and the related `qnt` and `rel` functions we'll look at
shortly) use the value of the `*scale*` variable. If you want to use a different
scale, you can change the value of the `*scale*` variable:
:::

```extempore
(set! *scale* (pc:scale 0 'phrygian))
```

This will take effect immediately for all patterns which use the `*scale*`
variable under the hood---if you've still got the `asc-scale` pattern running
you'll hear it straight away.

There are a couple of other functions which make use of this: `rel` for
calculating relative pitch values according to a scale and `qnt` for
"quantizing" (i.e. "snap-to-grid") a pitch to the current scale.

Here's an example of using relative intervals relative to middle C (`60`) rather
than absolute pitch numbers to play the start of a familiar melody (feel free to
set the tempo to your liking).

```extempore
(:> got 3 0 (play syn1 (rel 60 @1) 80 dur) '(4 0 (2 3)))
```

However, if you want a pattern which uses `scale`, `rel` and `qnt` to use a
different scale (i.e. not the current value of the global `*scale*` variable)
you can provide an optional third argument function. So if the previous pattern
has too dark a vibe for you, you can play it in a major key:

```extempore
(:> got 3 0 (play syn1 (rel 60 @1 (pc:scale 0 'ionian)) 80 dur) '(4 0 (2 3)))
```

The `qnt` function works the same way, except that instead of calculating a
relative pitch it just takes a pitch value as input and finds the nearest note
within the given scale. Compare:

```extempore
(:> asc-scale 4 0 (play syn1 @1 80 dur) (range 60 72))
(:> asc-scale 4 0 (play syn1 (qnt @1) 80 dur) (range 60 72))
```

Obviously these simple examples just scratch the surface of the possibilities
here---make some noise and experiment with what sounds good to you.

### Holders

Everything so far has been deterministic---the pattern list(s) are always the
same, so the sequence of pitches is always the same each time through the loop.
Sometimes you want a bit of randomness, though---here's one way to do it.

```extempore
(:> rand-pat 2 0 (play syn1 @1 80 dur) (list (random '(60 67 70))
                                             (random '(60 67 70))
                                             (random '(60 67 70))
                                             (random '(60 67 77))))
```

These four calls to `random` choose from the same three pitches (`60`, `67` and
`70`). There's a nice shortcut for this sort of thing called `nof` (i.e. give me
_n_ of this thing):

```extempore
(:> rand-pat 2 0 (play syn1 @1 80 dur) (list (nof 4 (random '(60 67 70)))))
```

We can even fake a "weighted" random sample by duplicating elements.

```extempore
(:> rand-pat 2 0 (play syn1 @1 80 dur) (list (nof 4 (random '(60 60 60 67 70)))))
```

However, having the 4 pitches change _every_ time through the pattern is a bit
jarring; we'd really prefer some sort of balance---pick four pitches at random,
loop through them a few times, then pick a new set of four pitches at random,
then loop through them a few times, etc.

This is where pattern holders come in handy. Unlike all the other things we've
looked at in the pattern language, to use a holder we need to define it
_outside_ the pattern first. Other than that, they look pretty similar to the
`cycle` function we saw before

```extempore
(define h1 (holder))
(:> rand-pat 2 0 (play syn1 @1 80 dur) (list (hold h1 4 (nof 4 (random '(60 60 60 61 67 70))))))
```

Note that we just wrapped the `(nof 4 ...)` expression in a `(hold h1 4 ...)`
one; so the "generate 4 new pitches for the pattern expression" thing will only
happen once every four loops. And obviously this is handy when you put it
against a 

You can define & use as many holders as you like, just make sure they all have
distinct names (e.g. `h1`, `h2`).

## FAQ

### Where can I see more examples?

Extempore ships with a bunch of examples of this stuff in use. There's the
`examples/sharedsystem/pattern_basics.xtm` file to start with (which covers
similar ground to this guide). But there's also a (growing) collection of
"covers" in `examples/sharedsystem/covers/` which show the pattern language in
action. You've already got them on your system, so open up that folder and take
a look 😉

### Why isn't my pattern working?

First, if it's not working you should check the terminal where Extempore is
running---this is where any errors will be logged. You might have a syntax
error, or be trying to use a variable which isn't defined, or any of the usual
frustrating programming errors. Don't lose heart, this stuff happens to
everyone.

However, there could also be a problem with the "musical" aspect of the pattern,
e.g. your're passing in pitches which are too low for your speakers to reproduce
(or for a human to hear) like `1`, `2`, `3`, etc. This is tricky because from a
"programming" perspective everything's working fine, but from a musical
perspective it's totally broken (unless you're into subsonic sound art, which is
totally fine).

If you want to inspect the values as they pass through the pattern list, the key
is to remember that the pattern expression is just a regular expression, so you
can use e.g. print statements with your special variables like this.

```xtlang
(:> test-pattern 4 0 (println LC LP @1 @2) '(1 2 3 4) '(10 20 30))
```

### I'm bored of my pattern(s), what can I do to make the more interesting?

Here are a few other list-processing functions which come in handy for
generating pattern lists---try them out and see what they sound like.

- `jumble`: randomize a list, e.g. `(jumble '(1 2 3 4))`
- `rotate`: cycle elements from the front of the list to the end, or vice versa, e.g. `(rotate '(1 2 3 4) 1)` => `(4 1 2 3)`, `(rotate '(1 2 3 4) -1)` => `(2 3 4 1)`
- `zip`: interleave two lists, e.g. `(zip '(1 2) '(3 4))` => `(1 3 2 4)`
- `pedal`: similar to zip, but insert a "pedal point" into a second list , e.g. `(pedal 1 '(2 3))` => `(1 2 1 3)`
- `take`: get just the first `n` elements of a list, e.g. `(take 2 '(1 2 3 4))` => `(1 2)`
- `flatten`: flatten a nested list e.g. `(flatten '((1) (2 ((3))) 4))` => `(1 2 3 4)`

### I'm so sick of the default chiptune synth sound, how do I change it? {#changing-the-sound}

This is covered in the [analogue-synth tutorial](analogue-synth.md), but if you can load a synth preset with
`analogue-load-preset` like so:

```xtlang
(analogue-load-preset syn1 "examples/sharedsystem/presets/organ1.xtmpreset")
```

Currently Extempore ships with the following presets (all in the
`extempore/sharedsystem/presets/` folder).

- `arp1.xtmpreset`
- `blade1.xtmpreset`
- `dr_bass.xtmpreset`
- `dr_fx.xtmpreset`
- `dr_fx2.xtmpreset`
- `dr_fx3.xtmpreset`
- `dr_lead.xtmpreset`
- `dr_lead2.xtmpreset`
- `dr_lead3.xtmpreset`
- `keys1.xtmpreset`
- `keys2.xtmpreset`
- `organ1.xtmpreset`
- `oxyarp.xtmpreset`
- `oxybass.xtmpreset`
- `oxylead.xtmpreset`
- `pad1.xtmpreset`
- `piano.xtmpreset`

### I see `cosr` in a lot of Andrew/Ben's Extempore performances---what's that about? {#what-is-cosr}

`cosr` (and cousins `sinr`, `trir`, `rectr` and `rampr`) are macros which
provide a beat-based LFO to modulate parameters. For example, to generate a
series of values centred at 50 which oscillate (sinusoidally) between 30 and 70,
over a period of 2 beats, you could use the following `cosr`:

```xtlang
(cosr 50 20 1/2)
```

`cosr` and friends are macros rather than functions because they rely on the
`beat` variable being defined in the enclosing environment. The `cosr` example
above actually expands out to:

```xtlang
(+ 50 (* 20 (cos (* TWOPI beat 1/2))))
```

If you use it inside either a pattern (or a [temporal recursion](../overview/time.md#temporal-recursion)) then this will be fine, because
`beat` will be defined to be the current beat (as a rational number). If you
want to use it _outside_ of one of those contexts, make sure that you've bound
`beat` to a sensible value for your purposes. Otherwise, you'll get an error
message like:

```
stack-catch: ()
eval: unbound variable: beat
```

As to why there's a trailing `r` on all of the macro names, that knowledge has
been lost to the mists of time.

### The pattern language is so inexpressive---why can't it do _x_?

This section of the guide is down the bottom, but it's probably a question that
occurred to you earlier---why can't the pattern language do _x_? The answer is
that it's because Extempore's pattern language is deliberately designed to work
with regular Scheme lists. You can use all the rest of the Extempore "world"
inside these patterns---you can pass higher-order functions, you can define and
call your own library code (as long as it returns lists), you can call
[xtlang](../reference/scheme-xtlang-interop.md)---there's nothing you can do in Extempore that you can't do inside a pattern.

So Extempore's pattern language isn't really a
[DSL](https://en.wikipedia.org/wiki/Domain-specific_language), it's more of a
pseudo-DSL, another example in the long LISP tradition of sneaking DSLs into a
full-fledged language environment. There are pros and cons to this (of which
songs have been sung and wars fought) but that's the reason it's the way is is.

### How can I learn Scheme properly?

There are lots of good (free) online resources for learning Scheme. One good
option is _The Scheme Programming Language_ by R. Kent Dybvig, which is [freely
available online](https://www.scheme.com/tspl4/). You could also work through
the famous _Structure and Interpretation of Computer Programs, by Abelson,
Sussman, and Sussman_ (often referred to as SICP), again [freely available
online](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html).

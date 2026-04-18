
---
title: audio_dsp
---
(sys:load "libs/core/audio_dsp.xtm")   	 

# Introduction

The audio_dsp package provides core dsp utilities, helper functions and generators for audio. 

NOTE: SAMPLE is a type alias for float. However, it also tells you that this function expects a value between 0.0 and 1.0

# Useful Math Functions

## dsp_randn
```extempore
dsp_randn:[float]*
```
### Returns
+ **SAMPLE** : A random number between 0.0 and 1.0

### Description
This returns a Gaussian distributed random number between 0.0 and 1.0

It uses the Marasaglia algorithm (http://c-faq.com/lib/gaussian.html).

**TODO** There may be a bug in the implementation... Need to look at this again. This feels suboptimal. Why call dsp_randn, and that's not going to change the values here. Think there's a better solution.

## parabolicSineHP
```extempore
parabolicSineHP:[SAMPLE,SAMPLE]*
```

### Parameters
1. **x:SAMPLE** : The x value for a Parabolic Sine function.

### Returns
+ **SAMPLE** : The y value for this function.

## Description
A highly accurate implemntation of the Parabolic Sine function. If accuracy is less important than speed, there is also a low quality implementation of this function available to you.

## parabolicSineLP
```extempore
parabolicSineLP:[SAMPLE,SAMPLE]*
```

### Parameters
1. **x:SAMPLE** : The x value for a Parabolic Sine function.

### Returns
+ **SAMPLE** : The y value for this function.

### Description
A low quality, but fast, implemntation of the Parabolic Sine function. If accuracy is more important than speed, there is a highly accurate implementation of this function available to you.


## fasttan
```extempore
fasttan:[SAMPLE,SAMPLE]*
```

### Parameters
1. **x:SAMPLE** : The x value for a tan function.

### Returns
+ **SAMPLE** : The y value for this function.

### Description
A fast implemntation of tan.

**TODO** What are the compromises in using this? Is there a range of acceptable values.

## fasttanh
```extempore
fasttan:[SAMPLE,SAMPLE]*
```

### Parameters
1. **x:SAMPLE** : The x value for a tanh function.

### Returns
+ **SAMPLE** : The y value for this function.

### Description
A fast implemntation of tanh.

**TODO** What are the compromises in using this? Is there a range of acceptable values.

## fastsin
```extempore
fasttan:[SAMPLE,SAMPLE]*
```

### Parameters
1. **x:SAMPLE** : The x value for a tanh function. X must be within the range -PI <= X <= PI

### Returns
+ **SAMPLE** : The y value for this function.

### Description
A fast implementation of sin. When calling this function make sure that you adjust x so that it lies within the range -PI <= X <= PI

# DSP Functions
## integrator_c

This is currently wrong, so I'm going to describe how it as if the bug was fixed.

```extempore
integrator_c:[[SAMPLE,SAMPLE]*]*
```

### Returns
+ **[SAMPLE,SAMPLE]** : The leaky integrator closure.

#### Closure Parameters
1. **x:SAMPLE** : The signal we wish to integrate.

#### Closure Return Value
+ **SAMPLE** : The current sum.

### Description
**TODO** Cuurrently this has a bug, so fix it.

This function returns a leaky integrator closure.

### Example
**TODO** Show how to create it and then used it.

# Oscillators (Aliasing)

These oscillators are useful for control level modulation, or situations where you're not worried about aliasing. Otherwise use the band-limited oscillators described in the next section.

## sin_c
```extempore
osc_c:[[SAMPLE,SAMPLE,SAMPLE]*,SAMPLE]*
```

**TODO** I feel this should probably have sin in there somewhere.

### Parameters
1. **phase:SAMPLE** : The initial phase (radians) for the oscillator.

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE]*__ : The Sin Oscillator closure.

#### Closure Parameters
1. **amp:SAMPLE** : Amplitude of the oscillator.
2. **freq:SAMPLE** : Current frequency of the oscillator. 

#### Closure Return Value
+ **SAMPLE** : The output signal for the oscillator.

### Description
This function creates an aliasing Sine oscillator.

### Example
**TODO** One example should be a cosine.

## saw_c
```extempore
saw_c:[[SAMPLE,SAMPLE,SAMPLE]*,SAMPLE]*
saw_c:[[SAMPLE,SAMPLE,SAMPLE]*,SAMPLE,i1]*
```

### Parameters
1. **mod:SAMPLE** : The initial phase where 0 <= mod <=1
2. **inverted:i1** _optional_ : If #t then the saw tooth is inverted. The default is #f (i.e. a normal rising saw tooth).

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE]*__ : The Saw Tooth Oscillator closure.

#### Closure Parameters
1. **amp:SAMPLE** : Amplitude of the oscillator.
2. **freq:SAMPLE** : Current frequency of the oscillator. 

#### Closure Return Value
+ **SAMPLE** : The output signal for the oscillator.

### Description
This function creates an aliasing Sawtooth oscillator.

### Example
**TODO** One example should be an inverted saw tooth.

## pulse_c
```extempore
pulse_c:[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*,SAMPLE]*
```

### Parameters
1. **mod:SAMPLE** : The initial phase where 0 <= mod <=1

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The Pulsewave Oscillator closure.

#### Closure Parameters
1. **amp:SAMPLE** : Amplitude of the oscillator.
2. **freq:SAMPLE** : Current frequency of the oscillator. 
3. **pw:SAMPLE** : The pulse width where 0 <= pw <= 1.0

#### Closure Return Value
+ **SAMPLE** : The output signal for the oscillator.

### Description
This function creates an aliasing pulsewave oscillator.

### Example
**TODO**

## tri_c
```extempore
tri_c:[[SAMPLE,SAMPLE,SAMPLE]*,SAMPLE]*
```

### Parameters
1. **mod:SAMPLE** : The initial phase where 0 <= mod <=1

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE]*__ : The Triangle Oscillator closure.

#### Closure Parameters
1. **amp:SAMPLE** : Amplitude of the oscillator.
2. **freq:SAMPLE** : Current frequency of the oscillator. 

#### Closure Return Value
+ **SAMPLE** : The output signal for the oscillator.

### Description
This function creates an aliasing triangle oscillator.

### Example
**TODO**

# Anti-Aliased Oscillators
These oscillators are anti-aliased, but consume more resources. Use these for audio signals, but they're probably overkill for most control signals (unless you're modulating at audio rate).

## blsaw_c
```extempore
blsaw_c:[[SAMPLE,SAMPLE,SAMPLE]*,SAMPLE]*
blsaw_c:[[SAMPLE,SAMPLE,SAMPLE]*,SAMPLE,i1]*
blsaw_c:[[SAMPLE,SAMPLE,SAMPLE]*,SAMPLE,i1,i1]*
```

### Parameters
1. **mod:SAMPLE** : The initial phase where 0 <= mod <=1
2. **inverted:i1** _optional_ : If #t then the saw tooth is inverted. The default is #f (i.e. a normal rising saw tooth).
3. **high_cpu** _optional_ : If set to true this will use a better, but slower, algorithm. The default value is #f.

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE]*__ : The Saw Tooth Oscillator closure.

#### Closure Parameters
1. **amp:SAMPLE** : Amplitude of the oscillator.
2. **freq:SAMPLE** : Current frequency of the oscillator. 

#### Closure Return Value
+ **SAMPLE** : The output signal for the oscillator.

### Description
This function creates an anti-aliased Sawtooth oscillator. It is implemented using the BLEP method.

### Example
**TODO** One example should be an inverted saw tooth.


## blpulse_c
```extempore
blpulse_c:[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*,SAMPLE,i1]*
```

### Parameters
1. **mod:SAMPLE** : The initial phase where 0 <= mod <=1
2. **highcpu:i1** _optional_ : If set to true this will use a better, but slower, algorithm. The default value is #t (TODO really?).
### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The Pulsewave Oscillator closure.

#### Closure Parameters
1. **amp:SAMPLE** : Amplitude of the oscillator.
2. **freq:SAMPLE** : Current frequency of the oscillator. 
3. **pw:SAMPLE** : The pulse width where 0 <= pw <= 1.0

#### Closure Return Value
+ **SAMPLE** : The output signal for the oscillator.

### Description
This function creates an anti-aliased pulsewave oscillator.

It uses the sum of saws method.

### Example
**TODO**

## bltri_c
```extempore
bltri_c:[[SAMPLE,SAMPLE,SAMPLE]*,SAMPLE]*
```

### Parameters
1. **mod:SAMPLE** : The initial phase where 0 <= mod <=1

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE]*__ : The triangle oscillator closure.

#### Closure Parameters
1. **amp:SAMPLE** : Amplitude of the oscillator.
2. **freq:SAMPLE** : Current frequency of the oscillator. 

#### Closure Return Value
+ **SAMPLE** : The output signal for the oscillator.

### Description
This function creates an anti-aliased triangle oscillator. TODO - uses which method?

### Example
**TODO**


# Noise Generators
## white_c
```extempore
white_c:[[SAMPLE]*]*
```

### Returns
+ __[SAMPLE]*__ : White noise closure.

#### Closure Return Value
+ **SAMPLE** : The output signal for the oscillator.

### Description
This function creates a white noise generator. The white noise is gaussian distributed (see dsp_randn).

### Example
**TODO**

## pink_c
```extempore
pink_c:[[SAMPLE]*]*
```

### Returns
+ __[SAMPLE]*__ : Pink noise closure.

#### Closure Return Value
+ **SAMPLE** : The output signal for the oscillator.

### Description
This function creates a pink noise generator. The pink noise is generated using Paul Kellet's economy method: http://www.firstpr.com.au/dsp/pink-noise/

### Example
**TODO**

# Interpolators

## LinTerp

```extempore
LinTerp:[SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE]*
```

### Parameters
1. **x1:SAMPLE** : The x value _before_ the interpolated value.
2. **x2:SAMPLE** : The x value _after_ the interpolated value.
3. **y1:SAMPLE** : The y value _before_ the interpolated value.
4. **y1:SAMPLE** : The y value _after_ the interpolated value.
5. **x:SAMPLE** : The x that we want an interpolated value for.

### Returns
+ __SAMPLE__ : interpolated ```y``` value.

### Description

Given the points (x1,y1) and (x2,y2) this will return the interpolated value y when given an x that lies between x1 and x2.

### Example

## hermite_interp

```extempore
hermite_interp:[SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE]*
```

### Parameters
1. **fractional:SAMPLE** : TODO.
2. **y1:SAMPLE** : TODO
3. **x0:SAMPLE** : TODO
4. **x1:SAMPLE** : TODO
5. **x2:SAMPLE** : TODO

### Returns
+ __SAMPLE__ : interpolated ```y``` value.

### Description

TODO
### Example

# Conversion Utilities

## midi2frq

```extempore
midi2frq:[float,float]*
```

### Parameters
1. **float** : A midi note number 

### Returns
- **float** : A frequency in herz

### Description
Converts a midi value to a frequency value. E.g. given 60 it will return the value 261.6 Hz

### Example

```
> ($ (midi2frq 60.0))
261.625549
```

## frq2midi

```
frq2midi:[float,float]*
```

### Parameters
1. **float** : A frequency in herz 

### Returns
- **float** : A midi note number

### Description
Converts a frequency value to a midi value. E.g. given 261.6 it will return the value 60.

### Example

```extempore
> ($ (midi2frq 261.625549))
60
```

## bpm2hz

```extempore
bpm2hz:[float,float]*
```

### Parameters
1. **float** : Beats per Minute 

### Returns
- **float** : A frequency in herz

### Description

Converts a value in beats per minute to a frequency in herz.

### Example

```extempore
> ($ (bpm2hz 60))
1.000
```

## hz2bpm

```extempore
hz2bpm:[float,float]*
```

### Parameters
1. **float** : A frequency in herz 

### Returns
- **float** : Beats per minute

### Description

Converts a value in herz to beats per minute.

### Example

```extempore
> ($ (hz2bpm 1.000))
60.000
```

## amp2db

```extempore
amp2db:[float,float]*
```

### Parameters
1. **float** : Volume amplitude Amps where 0.0 < Amps <= 1.0

### Returns
- **float** : Volume in decibels where -inf < db <= 0.0

### Description

A utility function that converts between Extempore's model of volume amplitude and decibels.

Amplitude in Extempore is a linear scale between 0.0 and 1.0, where 0.0 is silence and 1.0 is the maximum value. 

### Example

```extempore
> ($ (amp2db 0.5))
-6.0206
```

## db2amp

```extempore
db2amp:[float,float]*
```

### Parameters
1. **float** : Volume in decibels where -inf < db <= 0.0

### Returns
- **float** : Volume amplitude Amps where 0.0 < Amps <= 1.0

### Description

A utility function that converts from decibels (where 0.0 is the maximum possible volume) to Extempore's model of volume amplitude and decibels.

Amplitude in Extempore is a linear scale between 0.0 and 1.0, where 0.0 is silence and 1.0 is the maximum value. 

### Example

```extempore
> ($ (db2amp -6.0206))
0.5
```


# Envelope Followers

These are closures that are useful for monitoring and manipulating volume.

## rms_c

```extempore
rms_c:[[SAMPLE,SAMPLE]*]*
```

### Returns
- __[SAMPLE,SAMPLE]*__ : The closure we will use for our DSP chain.

#### Closure Parameters
1. **SAMPLE** : current sound sample from input source.

#### Closure Environment
- **db:SAMPLE** : The RMS value for the current FRAME. Note that this is in decibels.

#### Returns
- **SAMPLE** : The input value (e.g. the signal is passed straight through)

### Description

This function creates a closure that takes a signal and calculates the RMS value for it.

The RMS value is calculated every frames (e.g. the global variable FRAMES which defines extempore's block size) and placed in the closure's environment as the value ```db```. 

The signal is passed through the closure unchanged. This allows you to easily place it in a dsp chain without worrying about the signal.

*DB is (RMS is +3db - i.e. dBFS with sine 1.0 at 0.0db)* - need a better explaination TODO.

### Example

**TODO**

# Envelopes

## gainf

```extempore
gainf::[SAMPLE,i64,i64,SAMPLE]*
```

### Parameters
1. **time:i64** : elapsed time since the envelope curve was triggered. The curve starts at time 0.
2. **width:i64** : the duration of the envelope curve in *TODO*.
3. **power:float** : This controls the curve of the envelope curve. The envelope curve can be flat, linear or have a quadratic curve.

### Returns
- **SAMPLE** : The current value of the generated envelope curve. **TODO** RANGE?

### Description

Generates an envelope curve. The envelope curve has duration ```width```, and given a time value between 0 and width **TODO** will return a value from within the curve.

**TODO** The value will always be between 0 and 1.0

The ```power``` parameter determines the shape of the curve. Positive values will give you a curve that goes up, negative values a curve that goes down.

#### Power Values
If you want an envelope that descends, rather than ascends, use a negative power value. 
- **0.0 -> 1.0** : The output will always be ```power```. E.g., if you set ```power``` to 0.5, you will always get an output of 0.5.
- **1.01 -> 2.0** : This will give you an ascending line from 0 to the mantissa **TODO** check. So 1.5 will give you an ascending line from ```0.0``` to ```0.5``` over time ```width```.
- **2.01 and higher** : These will give you ascending curves of increasing steepness. If the value is negative this will give you a descending curve that is the mirror of the ascending curve.

### Example

**TODO**. Needs several probably.

# Multi Channel (E.G. Stero)

## panner

```extempore
panner::[SAMPLE,i64,SAMPLE]*
```

### Parameters
1. **channel:i64** : The channel that you want the correct value for.
2. **pan:SAMPLE** : The stereo pan, where 0.0 is on the left, 0.5 is the middle and 1.0 is the right.

### Returns
- **SAMPLE** : The properly panned value for this channel **TODO** how does it calculate this?

;; given current channel
;; and pan value [0.0-1.0]
;; returns value betwee n [0.0-1.0]
;; uses CHANNELS

### Description

Given a ```sample``` input and a desired ```pan``` level this function will calculate the correct output for the desired ```channel```.

**TODO** How is this calculated?

### Example

**TODO**. Needs several probably.

# Signal Functions
## hann
```extempore
panner::[SAMPLE,i64,i64,i64]*
```

### Parameters
1. **time:i64** : elapsed time since this function started (0 is the start time)
2. **width:i64** : Width of the hann window in *TODO* time periods
3. **repeat:i64** : repeat period. 1 is immediately, 2 is after a single cycle, 5 is after 4 cycles. **TODO** 0 should be never repeat.

### Returns
+ **SAMPLE** - A value between 0.0 and 1.0, where 1.0 is the peak of the hann windows.

### Description

This generates a repeating hann window signal. Use repeat to define the repititon pattern. ```0``` for non-repeating, ```1``` for continuous, ```2``` for every other cycle, etc.

### Examples
**TODO**

### Code Update
Could remove entirely hann by simply setting the the repeat to 0.0

## impulse
```extempore
impulse::[SAMPLE,i64,i64]*
```

### Parameters
1. **time:i64** : time in samples starting from 0.
2. **apex:i64** : time in samples after 0 when the impulse should be fired (**TODO** Note it can't be 0 - which is odd). Until ```apex``` is reached the output will be 0.

### Returns
+ **SAMPLE** - A value between 0.0 and 1.0 for the impulse value

### Description

This generates an impulse of 1.0 that begins at time period apex and outputs 1.0 for a single sample period. After it is activated it will exponentially decay to 0.

### Examples
**TODO**

# DSP Utility 

## poly_blep
```extempore
poly_blep:[float,float,float,i1]*
```

### Parameters
1. __pos:SAMPLE__ : TODO
2. __inc:SAMPLE__ : TODO
3. __height:SAMPLE__ : TODO
4. __rising_edge:i1__ : TODO

### Returns
+ __SAMPLE__ : TODO

### Description
A polynomial bandlimited step function. TODO


## blep_n
```extempore
blep_n:[float,float*,float,float,float,float,i1,float,i1]*
```

### Parameters
1. __table:SAMPLE*__ : TODO
2. __tablenlen:SAMPLE__ : length of table.
3. __mod:SAMPLE__ : TODO
4. __inc:SAMPLE__ : TODO
5. __height:SAMPLE__ : TODO
6. __rising:i1__ : TODO
7. __num_points:SAMPLE__ : TODO
8. __interp:i1__ : TODO

### Returns
+ __SAMPLE__ : TODO

### Description
TODO

# Miscellaneous

## print_audio_state

```
print_audio_state:[void]*
```

### Description

Prints the status of the soundcard to the command line.

# Filters

Many of the functions that create a filter closure expect an enumeration value. These are defined below:
+ **LPF1** : Low Pass Filter, 1 pole.
+ **LPF2** : Low Pass Filter, 2 poles.
+ **LPF4** : Low Pass Filter, 4 poles.
+ **HPF1** : High Pass Filter, 1 pole.
+ **HPF2** : High Pass Filter, 2 poles.
+ **HPF4** : High Pass Filter, 4 poles.
+ **BPF1** : Band Pass Filter, 1 pole.
+ **BPF2** : Band Pass Filter, 2 poles.
+ **BPF4** : Band Pass Filter, 4 poles.

## lpf_1p_c
```extempore
lpf_1p_c::[[SAMPLE,SAMPLE,SAMPLE]*]*
```

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE]*__ : The low pass filter closure.

#### Closure Parameters
1. __x:SAMPLE__ : input sample that we are filtering
2. __g:SAMPLE__ : positions the pole on the unit axis (0.0 -> 1.0)

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
A simple 1 pole filter. g moves the pole along the axis. TODO - check that you're remembering this correctly.


## va_onepole
```extempore
va_onepole::[[SAMPLE,SAMPLE,SAMPLE]*,i32]*
```

### Parameters
1. __type:i32__ : The type of filter that is being created. You choices are LPF1 (for a low pass filter) and HPF1 (for a high pass filter).

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering
2. __freq:SAMPLE__ : filter cutoff frequency.
3. __q:SAMPLE__ : filter 'q' value

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
A virtual analog modelled 1 pole filter. This can either be a low pass filter, or a high pass filter.

Based upon Will Pirkle's implementation.

## moog_ladder
```extempore
moog_ladder::[[SAMPLE,SAMPLE,SAMPLE]*,i32]*
```

### Parameters
1. __type:i32__ : The type of filter that is being created. You choices are LPF2 and LPF4 (for a low pass filter), BPF2 and BPF4 (for a band pass filter) and HPF2 and HPF4 (for a high pass filter).

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering
2. __freq:SAMPLE__ : filter cutoff frequency.
3. __q:SAMPLE__ : filter 'q' value

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
The legendary Moog filter. This can serve as a low pass, band pass and high pass filter. [TODO would be good to reference which one of the many designs this is, and it's performance generally].


## svf_c
```extempore
svf_c::[[|6,float|*,float,float,float,|6,float|*]*]*
```

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,|6,float|*]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __cutoff:SAMPLE__ : filter cutoff frequency.
3. __res:SAMPLE__ : Filter resonance (q).
4. __outarray:|6,float|*__ : Memory location to store the output.

#### Closure Returns
+ __|6,float|*__ : The output from the different components of this filter: |low pass, band pass, high pass, notch filter, peak filter, allpass filter|

### Description
An analog modelled state variable filter. State variable filters produce multiple filter responses from the same circuitry.

This design follows: http://www.cytomic.com/files/dsp/SvfLinearTrapOptimised2.pdf. The 'using v1 to compute v2' version.


## lpf_c
```extempore
lpf_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*]*
```

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __cutoff:SAMPLE__ : filter cutoff frequency.
3. __res:SAMPLE__ : Filter resonance (q).

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO - more info. Essentially this is a state variable filter version of a low pass filter. Not sure if this is analog modelled, or not.

## lpf_c
```extempore
lpf_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*]*
```

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __cutoff:SAMPLE__ : filter cutoff frequency.
3. __res:SAMPLE__ : Filter resonance (q).

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO - more info. Essentially this is a state variable filter version of a low pass filter. Not sure if this is analog modelled, or not.

## lpf2_c
```extempore
lpf_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*]*
```

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __cutoff:SAMPLE__ : filter cutoff frequency.
3. __res:SAMPLE__ : Filter resonance (q).

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO - more info. Essentially this is a state variable filter version of a low pass filter. Not sure if this is analog modelled, or not.

This is less efficient that lpf_c, but this one could be run on two processors. Currently this is probably more interesting rather than useful.

## hpf_c
```extempore
hpf_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*]*
```

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __cutoff:SAMPLE__ : filter cutoff frequency.
3. __res:SAMPLE__ : Filter resonance (q).

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO - more info. Essentially this is a state variable filter version of a high pass filter. Not sure if this is analog modelled, or not.

## hpf_c
```extempore
hpf_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*]*
```

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __cutoff:SAMPLE__ : filter cutoff frequency.
3. __res:SAMPLE__ : Filter resonance (q).

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO - more info. Essentially this is a state variable filter version of a high pass filter. Not sure if this is analog modelled, or not.


## apf_c
```extempore
apf_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*]*
```

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __cutoff:SAMPLE__ : filter cutoff frequency.
3. __res:SAMPLE__ : Filter resonance (q).

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO - more info. Essentially this is a state variable filter version of an all pass filter. Not sure if this is analog modelled, or not.

## notch_c
```extempore
hpf_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*]*
```

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __cutoff:SAMPLE__ : filter cutoff frequency.
3. __res:SAMPLE__ : Filter resonance (q).

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO - more info. Essentially this is a state variable filter version of a notch filter. Not sure if this is analog modelled, or not.

## peak_c
```extempore
peak_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*]*
```

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __cutoff:SAMPLE__ : filter cutoff frequency.
3. __res:SAMPLE__ : Filter resonance (q).

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO - more info. Essentially this is a state variable filter version of a peak filter. Not sure if this is analog modelled, or not.

## bell_c
```extempore
bell_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*]*
```

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __cutoff:SAMPLE__ : filter cutoff frequency.
3. __q:SAMPLE__ : Filter resonance (q).

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO - more info. Essentially this is a state variable filter version of a bell filter. Not sure if this is analog modelled, or not.

TODO: From the code. Q is usually around 0.5 gain is either a boost up to +5.0db or a reduction down to -24.0db. Little cryptic...

## lshelf_c
```extempore
lshelf_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*]*
```

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __cutoff:SAMPLE__ : filter cutoff frequency.
3. __q:SAMPLE__ : Filter resonance (q).

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO - more info. Essentially this is a state variable filter version of a low shelf filter. Not sure if this is analog modelled, or not.

TODO: From the code. Q is usually around 0.5 gain is either a boost up to +5.0db or a reduction down to -24.0db. Little cryptic...

## hshelf_c
```extempore
hshelf_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*]*
```

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __cutoff:SAMPLE__ : filter cutoff frequency.
3. __q:SAMPLE__ : Filter resonance (q).

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO - more info. Essentially this is a state variable filter version of a high shelf filter. Not sure if this is analog modelled, or not.

TODO: From the code. Q is usually around 0.5 gain is either a boost up to +5.0db or a reduction down to -24.0db. Little cryptic...



## skf_c
```extempore
skf_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*]*
```

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __cutoff:SAMPLE__ : filter cutoff frequency.
3. __q:SAMPLE__ : Filter resonance (q).

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
A low pass filter of the Sallen Key type.

TODO - more info probably.

Implemented using Andy Simper's Sallen Key LPF
http://cytomic.com/files/dsp/SkfLinearTrapOptimised2.pdf


## delay_apf_c
```extempore
delay_apf_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*,i64]*
```

### Parameters
1. __delay:i64__ : Delay time for this delay line.

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE]*__ : The all pass filter closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __apf_gain:SAMPLE__ : TODO What this actually does :)

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
All pass filter implemented using a delay.

# Delays
tap delays will also end up here.

# Comb Filters
## delay_c
```extempore
delay_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*,i64]*
```

### Parameters
1. __delay:i64__ : Delay time for this delay line.

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The delay closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __wet:SAMPLE__ : how much of the input signal should be mixed with the delay. 0.0 will give you 100% of ```in``` 1.0 will give you 100% of the delay (TODO Actually it doesn't do this, but shouldn't it?)
3. __feedback:SAMPLE__ : Feedback. In theory this should be how much of the out signal gets mixed into the delay with in. But that doesn't seem to be what's going on.

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
Non interpolated delay line. TODO: Work out how this actually works and check that it's correct (it probably is). It's possible that this is actually a comb filter, in which case I think the name should be comb_c, and just rely upon it having a non-fractional delay maybe? It is confusing.

## comb_c
```extempore
comb_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*,i64]*
```

### Parameters
1. __delay:i64__ : Delay time for this delay line.

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The delay closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __delay:SAMPLE__ : TODO Dig out the DSP books...
3. __mix:SAMPLE__ : how much of the input signal should be mixed with the delay. 0.0 will give you 100% of ```in``` 1.0 will give you 100% of the delay (TODO Actually it doesn't do this, but shouldn't it?)
3. __fb:SAMPLE__ : Feedback. In theory this should be how much of the out signal gets mixed into the delay with in. But that doesn't seem to be what's going on.

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
Interpolated comb filter. You can use fractional delay amounts and it will do the necessary math to give you the intended result.

## comb_lpf_c
```extempore
comb_lpf_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*,i64]*
```

### Parameters
1. __delay:i64__ : Delay time for this delay line.

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The delay closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __delay:SAMPLE__ : TODO Dig out the DSP books...
3. __mix:SAMPLE__ : how much of the input signal should be mixed with the delay. 0.0 will give you 100% of ```in``` 1.0 will give you 100% of the delay (TODO Actually it doesn't do this, but shouldn't it?)
3. __fb:SAMPLE__ : Feedback. In theory this should be how much of the out signal gets mixed into the delay with in. But that doesn't seem to be what's going on.
4. __cof:SAMPLE__ : cutoff frequency for the low pass.
5. __res:SAMPLE__ : resonance amount (TODO Q?)

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO: Interpolated comb filter with a low pass filter. I'm not sure why this is necessary, other than as a building block for other ugens. Feels redundant.

## comb_lpf_1p_c
```extempore
comb_lpf_1p_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*,i64]*
```

### Parameters
1. __delay:i64__ : Delay time for this delay line.

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The delay closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __delay:SAMPLE__ : TODO Dig out the DSP books...
3. __combg:SAMPLE__ : TODO dunno
3. __lpfg:SAMPLE__ : low pass filter gain (see lpf I guess).
4. __cof:SAMPLE__ : cutoff frequency for the low pass.
5. __res:SAMPLE__ : resonance amount (TODO Q?)

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO: Interpolated comb filter with a simple 1 pole low pass filter. I'm not sure why this is necessary, other than as a building block for other ugens. Feels redundant.

## comb_apf_c
```extempore
comb_apf_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*,i64]*
```

### Parameters
1. __delay:i64__ : Delay time for this delay line.

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The ugen closure.

#### Closure Parameters
1. __x:SAMPLE__ : input sample that we are filtering.
2. __delay:SAMPLE__ : TODO Dig out the DSP books...
3. __apf:gain__ : The all pass filter gain.

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO: Interpolated comb filter with a simple all pass filter filter. I'm not sure why this is necessary, other than as a building block for other ugens. Feels redundant.

## delay_apf_c
```extempore
delay_apf_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*,i64]*
```

### Parameters
1. __delay:i64__ : Delay time for this delay line.

### Returns
+ __[SAMPLE,SAMPLE,SAMPLE]*__ : The delay closure.

#### Closure Parameters
1. __in:SAMPLE__ : input sample that we are filtering.
2. __gain:SAMPLE__ : TODO Dig out the DSP books and work out what gain is doing here.

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO Assume this is an all pass filter that uses delay. Need to check.




# Reverb
Add the stereo reverb because that's a thing.
# Effects
# Flanger
## flanger_c
```extempore
flanger_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE]*,i64]*
```

### Parameters
1. __delay:i64__ : Max delay time for this flanger.

### Returns
+ __flanger_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The flanger closure.

#### Closure Parameters
1. __x:SAMPLE__ : The thing we're TODO I guess the channel? Seems a little inflexible
2. __in:SAMPLE__ : input? TODO
3. __out:SAMPLE__ : output? TODO
4. __delay:SAMPLE__ : delay in milliseconds
5. __range:SAMPLE__ : range in miliseconds
6. __feedback:SAMPLE__ : 0..1 (TODO what do this do?)

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO Describe what a flanger does. Probably.


# Chorus
## chorus_c
```extempore
chorus_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE]*,SAMPLE]*
```

### Parameters
1. __phase:SAMPLE__ : TODO Definition

### Returns
+ __[[SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The flanger closure.

#### Closure Parameters
1. __x:SAMPLE__ : The thing we're TODO I guess the channel? Seems a little inflexible
2. __in:SAMPLE__ : input? TODO
3. __out:SAMPLE__ : output? TODO
4. __feedback:SAMPLE__ : 0..1 (TODO what do this do?) feedbacks the output into the next signal almost certainly.

#### Closure Returns
+ __SAMPLE__ : filtered output sample.

### Description
TODO Describe what a chorus does. Probably.
# Delays
## tap_delay_c
```extempore
tap_delay_c::[[float,float]*,i64,i64]*
```
### Parameters
1. __max_delay:i64__ : Max length of this tap delay (e.g. 64)
2. __num_taps:SAMPLE__ : 
### Returns
+ __[[SAMPLE,SAMPLE,SAMPLE,SAMPLE,SAMPLE]*__ : The flanger closure.

#### Closure Parameters
1. __x:SAMPLE__ : The input sample

#### Closure Returns
+ __SAMPLE__ : processed output

### Description
TODO Describe what a tap delay does. I feel this could be done more flexibly just by creating a general purpose delay line that can easily be accessed. Also needs some more controls (decay, etc).
# Reverb
## reverb_c
```extempore
reverb_c::[[i64,SAMPLE,SAMPLE,float,float,SAMPLE,SAMPLE]*]*
```
### Returns
+ __[[i64,SAMPLE,SAMPLE,float,float,SAMPLE,SAMPLE]*__ : The flanger closure.

#### Closure Parameters
1. __chan:i64__ : The channel of the input (TODO need a better way of handling)
2. __x:SAMPLE__ : The input sample
3. __size:SAMPLE__ : roomsize from 0.0 to 1.0
4. __predelay:float__ : milliseconds. Can go as high as 2 seconds, but 40 milliseconds is a good starting point.
5. __absorb:SAMPLE__ : Amount of high frequency damping (simulating wall absorbtion). 0.0 to 1.0
6: __mix:SAMPLE__ : 0.0 dry to 1.0 wet

#### Closure Returns
+ __SAMPLE__ : processed output

### Description
A reverb. TODO describe topology, type, etc. Need more reverb.

## reverb_st_c
```extempore
reverb_st_c::[[i64,SAMPLE,SAMPLE,float,float,SAMPLE,SAMPLE]*]*
```
### Returns
+ __[[i64,SAMPLE,SAMPLE,float,float,SAMPLE,SAMPLE]*__ : The flanger closure.

#### Closure Parameters
1. __chan:i64__ : The channel of the input (TODO need a better way of handling)
2. __x:SAMPLE__ : The input sample
3. __size:SAMPLE__ : roomsize from 0.0 to 1.0
4. __predelay:float__ : milliseconds. Can go as high as 2 seconds, but 40 milliseconds is a good starting point.
5. __absorb:SAMPLE__ : Amount of high frequency damping (simulating wall absorbtion). 0.0 to 1.0
6. __mix:SAMPLE__ : 0.0 dry to 1.0 wet

#### Closure Returns
+ __SAMPLE__ : processed output

### Description
A reverb for stereo signal. A kludge apparently. TODO find out why.

# Bit Crusher
## crusher_c
```extempore
crusher_c::[[SAMPLE,SAMPLE,SAMPLE]*]*
```
### Returns
+ __[[SAMPLE,SAMPLE,SAMPLE]*__ : The bit crusher closure.

#### Closure Parameters
1. __in:SAMPLE__ : The input sample
2. __bits:SAMPLE__ : bit size after processing (TODO why is this a float?)

#### Closure Returns
+ __SAMPLE__ : processed output

### Description
A 'dodgy' bitcrusher (TODO why dodgy)

# Sample and Hold
## hold3_c
```extempore
hold3_c::[[SAMPLE,SAMPLE,SAMPLE]*]*
```
### Returns
+ __[[SAMPLE,SAMPLE,SAMPLE]*__ : The sample and hold closure.

#### Closure Parameters
1. __in:SAMPLE__ : The input sample
2. __h:SAMPLE__ : if h > 0.0 then continue to play the last held sample

#### Closure Returns
+ __SAMPLE__ : processed output

### Description
Sample and hold. TODO description.

# Amp Distortion
## distort_c
```extempore
distort_c::[[SAMPLE]*]*
```
### Parameters
+ __gain:SAMPLE__ : How much gain to apply to the input (TODO think this should be a closure parameter)

### Returns
+ __[[SAMPLE]*__ : The distortion closure.

#### Closure Parameters
1. __in:SAMPLE__ : The input sample

#### Closure Properties
+ __gain:SAMPLE__ : How much gain to apply to the input
+ __lim:SAMPLE__ : the level at which this should start to distort. (0.0-1.0)

#### Closure Returns
+ __SAMPLE__ : processed output

### Description
A very basic distortion ugen. 

# Saturation
# Envelopes etc

## fade_c
```extempore
fade_c::[[SAMPLE,SAMPLE,SAMPLE,SAMPLE]*]*
```

### Description
TODO: This function doesn't make sense to me. I feel there is a better way of achieving a linen. Shouldn't it have a gate? and why not pass in the time, rather than it having it's own internal sense of that?

# Ring Modulation
# Synths
## Granulator
This feels a little too complex for this file.
# Mixing
## panner_c
TODO maybe come back to this, but seems confusing as currently presented. need channels independent of sound for these to work well.

Why are there mix closures and functions? 
# Panners and mixers



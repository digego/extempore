Reading & writing audio files in Extempore
==========================================

Extempore’s ``libsndfile`` bindings [1]_ provide functionality for both
reading and writing audio files. Here’s a short example of how to both
read and write audio files.

First, load up the required libraries and create an audio file closure
with ``audiofile_c``

.. code:: extempore

      (sys:load "libs/external/sndfile.xtm")

      (bind-func dsp:DSP 1000000000  ;; allocate memory to store the audio file
        (let ((audiofile (audiofile_c "/Users/ben/Desktop/xtm-assets/peg.wav" 0 0)))
          (lambda (in time chan dat)
            ;; get the output sample
            (audiofile))))

      (dsp:set! dsp)  

``audiofile_c`` takes three arguments:

-  the name of the wav file to load
-  the offset into the file to start from (``0`` for the start of the
   file)
-  the number of samples to read (``0`` for the whole file)

=audiofile\ :sub:`c`\ = returns a closure (which is bound to
``audiofile``) which will, when called, return successive samples from
the audio file in such a way that ``audiofile`` can be called once per
output sample and will play through the audio file at normal speed. When
``dsp`` is compiled, the log view also prints some info about the
file [2]_:

.. code:: bash

    file name:     /Users/ben/Desktop/xtm-assets/peg.wav
    samplerate:    44100
    channels:      2
    samples read:  21202944

It’s important to realise that the playhead—the point in the file which
playback is ‘up to’—is stored internally to the ``audiofile`` closure,
and if you just call it back with no arguments it will gradually move
through the whole file (as it is doing in the above code). By default,
when the playhead gets to the end of the file it wraps to the start, so
the file playback will loop on forever.

To mess with this audio stream, let’s add some valve saturation-style
distortion to both channels:

.. code:: extempore

      (bind-func dsp:DSP 1000000000
        (let ((audiofile (audiofile_c "/Users/ben/Desktop/xtm-assets/peg.wav" 0 0))
              (saturationl (saturation_c))
              (saturationr (saturation_c)))
          (lambda (in time chan dat)
            (* 0.1 ;; to compensate for saturation boost
               (cond ((= chan 0)
                      (saturationl (audiofile) 1.0 0.9))
                     ((= chan 1)
                      (saturationr (audiofile) 1.0 0.9))
                     (else (convert 0.0 SAMPLE)))))))

Sounds saturated and messy—great. [3]_ Now, we want to write the
processed audio back to a wav file. To do this, we need to `allocate
some memory`_ to write the output samples into. Looking at the file info
which was printed out earlier (in particular ``samples read:
21202944``), we know that there are ``21202944`` samples in the input
file, so that’s how big we want our output buffer to be.

.. code:: extempore

      (bind-func dsp:DSP 1000000000
        (let ((audiofile (audiofile_c "/Users/ben/Desktop/xtm-assets/peg.wav" 0 0))
              (saturationl (saturation_c))
              (saturationr (saturation_c))
              (filesize 21202944)  ;; number of samples in the input file
              (out_buffer:double* (halloc filesize)) ;; allocate memory from the heap
              (index 0))
          (lambda (in time chan dat)
            (let ((out_sample
                   (* 0.1 ;; to compensate for saturation boost
                      (cond ((= chan 0.0)
                             (saturationl (audiofile) 1.0 0.9))
                            ((= chan 1.0)
                             (saturationr (audiofile) 1.0 0.9))
                            (else (convert 0.0 SAMPLE))))))
              ;; if it's not full, write the out_sample to the output buffer
              (if (< index filesize)
                  (pset! out_buffer index out_sample))
              ;; notify us when it's done
              (if (= index filesize)
                  (printf "Audio buffer full.\n"))
              ;; increment our index
              (set! index (+ index 1))
              ;; (optional) return the out sample for playback
              out_sample))))

Once we get the “Audio buffer full” notification, it’s time to write the
output buffer to a file. To do this, we use the ``write_audio_file``
function, which takes four arguments:

-  the filename
-  the number of frames (a frame is a full set of samples—one for each
   channel)
-  the number of channels
-  a pointer to the buffer of audio samples

Let’s add a function ``write_data`` to write the audio file:

.. code:: extempore

      (bind-func dsp:DSP 1000000000
        (let ((audiofile (audiofile_c "/Users/ben/Desktop/xtm-assets/peg.wav" 0 0))
              (saturationl (saturation_c))
              (saturationr (saturation_c))
              (filesize 21202944)  ;; number of samples in the input file
              (out_buffer:double* (halloc filesize))
              (index 0)
              (write_file (lambda (buffer)
                            (write_audio_data "/Users/ben/Desktop/xtm-assets/peg-processed.wav"
                                              (/ filesize 2) ;; num frames
                                              2              ;; num channels
                                              buffer)))) ;; audio data
          (lambda (in time chan dat)
            (let ((out_sample
                   (* 0.1 ;; to compensate for saturation boost
                      (cond ((= chan 0.0)
                             (saturationl (audiofile) 1.0 0.9))
                            ((= chan 1.0)
                             (saturationr (audiofile) 1.0 0.9))
                            (else (convert 0.0 SAMPLE)))))
              ;; if it's not full, write the out_sample to the output buffer
              (if (< index filesize)
                  (pset! out_buffer index out_sample))
              ;; notify us when it's done
              (if (= index filesize)
                  (begin (printf "Audio buffer full.\n")
                         (write_file out_buffer)))
              ;; increment our index
              (set! index (+ index 1))
              ;; (optional) return the out sample for playback
              out_sample))))

After the recording has played through, the index will have counted up
to ``filesize``: the number of samples in the file, and the
``write_file`` closure will be called with the output data pointer. Sure
enough, I hear the processed (saturated) output file when I listen to
the ``peg-processed.wav`` file.

There are lots of alternate ways to do this.

-  the sound can be processed offline (rather than in real-time inside
   the ``dsp`` callback)
-  you can use the similar ``audiofile_ptr_c`` to return pointers into
   the audio file buffer rather than the values themselves
-  you can write your own functions to either simplify or make the
   process more flexible.

You can see the low-level libsndfile functions involved if you look in
``libs/external/sndfile.xtm``.

But the primary workflow for writing audio files with Extempore is

#. write the audio samples to a buffer
#. write that buffer to a file using ``write_audio_data``

.. [1]
   The ``libsndfile`` library can be found at
   ``libs/external/sndfile.xtm``.

.. [2]
   If you’re interested, the file I’m using is ‘Peg’ from `Steely Dan’s
   Aja`_. It’s a great album.

.. [3]
   We had to wrap the ``0.0`` value in a ``convert`` call to get the
   types right, as discussed in another `post`_.

.. _allocate some memory: 2012-08-17-memory-management-in-extempore.org
.. _Steely Dan’s Aja: http://www.rollingstone.com/music/lists/500-greatest-albums-of-all-time-20120531/steely-dan-aja-20120524
.. _post: 2013-11-15-changing-from-doubles-to-floats-in-audio_dsp.org

The Extempore programming environment
=====================================

Welcome to Extempore's documentation site. It's a work in
progress---up till now things have been hosted on `Ben's blog`_ but
this is an attempt to do things right, using Sphinx_.

.. code-block:: extempore

  (bind-func dsp:DSP
    (lambda (in time chan dat)
      (* 0.1 (cos (* .1 (convert time))))))
  
  (dsp:set! dsp)

.. _Ben's blog: http://benswift.me/extempore-docs/
.. _Sphinx: http://sphinx-doc.org/

.. _getting-started-docs:

.. toctree::
   :maxdepth: 1
   :caption: Getting started

   quickstart
   install
   editor-support
   about-this-documentation

.. _extempore-environment-docs:

.. toctree::
   :maxdepth: 1
   :caption: Key concepts

   philosophy
   caas
   time
   c-xtlang-interop
   scheme-xtlang-interop
   concurrency
   memory

.. _xtlang-docs:

.. toctree::
   :maxdepth: 1
   :caption: xtlang

   types
   type-inference
   best-practices

.. _tutorials:

.. toctree::
   :maxdepth: 1
   :caption: Tutorials & Guides

   audio-signal-processing
   making-an-instrument
   note-level-music
   sampler
   graphics
   impromptu-users

.. _help-docs:

.. toctree::
   :maxdepth: 1
   :caption: Community

   getting-help
   contributing
   testing

Other useful things
===================

* :ref:`genindex`
* :ref:`search`


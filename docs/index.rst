The Extempore programming environment
=====================================

.. note:: This documentation is not yet ready for prime time, so
          don't rely on it yet. See :doc:`about-this-documentation`
          for more info.

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
   editor-support
   about-this-documentation

.. _extempore-environment-docs:

.. toctree::
   :maxdepth: 1
   :caption: Extempore programming environment

   philosophy
   caas
   time
   c-xtlang-interop
   scheme-xtlang-interop

.. _xtlang-docs:

.. toctree::
   :maxdepth: 1
   :caption: xtlang---the Extempore language

   type-system
   generics
   memory

.. _tutorials:

.. toctree::
   :maxdepth: 1
   :caption: Tutorials & Guides

   audio
   graphics

.. _help-docs:

.. toctree::
   :maxdepth: 1
   :caption: Community

   getting-help
   contributing

Other useful things
===================

* :ref:`genindex`
* :ref:`search`


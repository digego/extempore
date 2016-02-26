The Extempore programming environment
=====================================

.. code-block:: extempore

  (bind-func dsp:DSP
    (lambda (in time chan dat)
      (* 0.1 (cos (* .1 (convert time))))))
  
  (dsp:set! dsp)

Welcome to Extempore's documentation site. It's a work in
progress---up till now things have been hosted on `Ben's blog
<http://benswift.me/extempore-docs/>`_ but this is an attempt to do
things right, using `Sphinx <http://sphinx-doc.org/>`_.

These docs are generated from the reStructured text (``.rst``) files
in the ``docs/`` subdirectory in the Extempore source distribution. So
if you find problems, or can think of improvements, fork away on GH,
edit the doc source files and submit a pull request---we'd love these
docs to become a real community effort.

.. _getting-started-docs:

.. toctree::
   :maxdepth: 2
   :caption: Getting started

   quickstart
   editor-support

.. _xtlang-docs:

.. toctree::
   :maxdepth: 2
   :caption: xtlang---the Extempore language

   type-system
   generics
   binding-to-C-libs

.. _extempore-environment-docs:

.. toctree::
   :maxdepth: 2
   :caption: Extempore programming environment

   caas
   time
   scheme-xtlang-interop

.. _tutorials:

.. toctree::
   :maxdepth: 2
   :caption: Tutorials

   audio
   graphics

.. _community-docs:

.. toctree::
   :maxdepth: 2
   :caption: Community

   mailing-list
   contributing
   list-of-projects

Indices and tables
==================

* :ref:`genindex`
* :ref:`search`


.. Extempore documentation master file, created by
   sphinx-quickstart on Fri Feb 26 09:45:22 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

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
Contents:

.. toctree::
   :maxdepth: 2

Getting Started
---------------

- Quickstart guide
- editor support

xtlang - the Extempore language
-------------------------------

- type system
- generics
- binding to C libs

Extempore runtime
-----------------

- CaaS
- time
- Scheme-xtlang interop

Tutorials
---------

- Audio
- Graphics

Getting involved
----------------

- list of projects

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`


quickstart
==========

I get it, you're impatient---here's the bare minimum required to get
from zero to running Extempore code.

Installation
------------

.. note:: There are much more detailed instructions in :doc:`install`,
          so if you have any problems (or simply want to know what's
          going on) with the steps below then that's a good place to
          look.
          
The quickest way to get started is to download a `binary release`_,
unzip it and run ``extempore.exe`` from inside the ``extempore``
folder.

.. _binary release: https://github.com/digego/extempore/releases

If you're more of a build-from-source type, then :ref:`that's pretty
easy as well <build-from-source-doc>`.

Editor setup
------------

To write Extempore code you need a text editor, and there are
:doc:`Extempore "plugins" for several text editors <editor-support>`
---Atom, Emacs, Sublime Text and Vim.

If you don't have a favourite text editor, then `Atom`_ is probably a
good choice---it's free, available on all platforms and doesn't have
as steep a learning curve as some other editors. Head over to the
:ref:`Extempore Atom setup <atom-editor-setup>` docs to find out to
download and set it up for Extempore.

If you *do* have a favourite text editor, and it's one of the ones
mentioned above, then see the :doc:`editor support page <editor-support>` for instructions on
how to get started hacking Extempore code in your editor of choice.

.. _Atom: https://atom.io/

"Hello, World!"
---------------

Hello, World! is pretty straightforward in Extempore

.. code-block:: extempore

  (printf "Hello, World!")

"Hello, Sine!"
--------------

Since Extempore has multimedia programming as a core part of its DNA,
here's "Hello, Sine!"

.. code-block:: extempore

  (bind-func sine:DSP
    (lambda (in time chan dat)
      (* .1 (cos (* (convert time) .04)))))

  ;; tell Extempore to use `sine` as the audio output sink
  (dsp:set! sine)

"Hello, Triangle!"
------------------

"Hello, Triangle!" is a bit more complicated, since setting up the
OpenGL state machine requires a bit of boilerplate. See
``examples/external/shader-tutorials/triangle.xtm`` to get started.

Beyond "Hello..."
-----------------

These simple snippets gloss over some subtleties of what's going on.
But hey, if you've started quickly(ish), then this page has done its
job. To understand the subtleties, read the rest of the documentation
:)

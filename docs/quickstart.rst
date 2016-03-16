Quickstart
==========

I get it, you're impatient---here's the bare minimum required to get
from zero to running Extempore code.

Installation
------------

.. note:: There are much more detailed instructions in :doc:`install`,
          so if you have any problems (or simply want to know what's
          going on) with the steps below then that's a good place to
          look.
          
On **OSX** (with `homebrew`_) you can get the dependencies, build and
install Extempore and the full standard library with::

  brew tap benswift/extempore && brew tap homebrew/versions && brew install extempore --with-extended

.. _homebrew: http://brew.sh/


On **Linux** if you've got git, cmake and a c++ compiler, then you can
build Extempore and the full standard library with::

  git clone https://github.com/digego/extempore && mkdir extempore/cmake-build && cd extempore/cmake-build && cmake .. && make install && make aot

On **Windows**, download a `precompiled binary`_, unzip it and run
``extempore.exe`` from inside the ``extempore`` folder.

.. _precompiled binary: http://extempore.moso.com.au/extras/Extempore-0.6.0-win64.zip

.. note:: **March 2016** this Windows binary is currently a fair bit
          behind git HEAD, so some of this documentation will be wrong
          as well. We're hoping to have a new Windows binary release
          real soon---if you have any expertise in this area then
          `get in touch`_.

.. _get in touch: mailto:extemporelang@googlegroups.com

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
job. Explaining those subtleties is what the rest of the documentation
is for.

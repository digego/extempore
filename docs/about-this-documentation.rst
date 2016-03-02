About this documentation
========================

This documentation is generated using Sphinx_, using the rtd_
theme. Eventually we might even host it at `Read the Docs`_, but
currently the plan is for someone (probably Ben) to generate the
(static) HTML, and then host it on GH pages or something.
Still, since it's really all just plain (well, reStructured_) text
files in the ``docs/`` subdirectory it should be easy for other folks
to contribute---or at least easier than the current setup (with docs
on `Ben's blog`_).

.. _Sphinx: http://www.sphinx-doc.org/
.. _rtd: https://github.com/snide/sphinx_rtd_theme
.. _reStructured: http://www.sphinx-doc.org/en/stable/rest.html
.. _Read the Docs: http://readthedocs.org
.. _Ben's blog: http://benswift.me/extempore-docs/

Generating the docs
-------------------

To generate these docs, you'll need a few python packages, something
like::

    pip install sphinx sphinx-autobuild sphinx_rtd_theme recommonmark

The docs are generated from the reStructured text (``.rst``) files
in the ``docs/`` subdirectory in the Extempore source distribution.
You can edit those files and build the documentation locally if you've
got all the Sphinx stuff on your box, just run::

    sphinx-autobuild . _build

in the ``docs/`` subdirectory.

Contributing to the docs
------------------------

If you find problems, or can think of improvements, `fork away on
GH`_, edit the documentation source files and submit a pull
request---we'd love these docs to become a real community effort.

If you've got questions, or want to bounce around some ideas for
improvements before you go ahead and make big changes then get in
touch on the `mailing list`_.

.. _fork away on GH: https://github.com/digego/extempore
.. _mailing list: mailto:extemporelang@googlegroups.com

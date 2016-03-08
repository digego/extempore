Text editors
============

.. note:: If you don't have a favourite text editor, or don't really
          know what a text editor is, then that's ok! Atom is probably
          the text editor for you.

.. _atom-editor-setup:

Atom
----

`Atom`_ is a cross-platform text editor which runs on OS X, Linux and
Windows. It was originally created by GitHub, but it's open source a
large (and growing) variety of packages for different languages and
tasks and a great community.

This doesn't cover all the basics of Atom here, because the `Atom
documentation`_ do that pretty well. Think of this as a 'cheat sheet',
for dipping back into to refresh your memory when you just can't
remember how to do something off the top of your head.

.. _Atom: https://atom.io/
.. _Atom documentation: https://atom.io/docs

Installation
^^^^^^^^^^^^

Installing Atom is a piece of cake---go to the `Atom homepage`_ and hit
the big red "Download" button near the top of the page. When it's
finished downloading, click on the downloaded file and follow the
instructions which show up on the screen.

.. _Atom homepage: https://atom.io/

Once you've got Atom up and running, you need to install the `Atom Extempore
package`_---this tells the Atom editor how to deal with Extempore code.
You can do this through the menu bar: ``Packages > Settings View >
Install Packages/Themes``, then search for "extempore" and click install
on the ``Extempore-Atom`` package. Alternatively, you can install it at
a terminal with ``apm install extempore-atom``. Once that's done, you're
ready to go.

.. _Atom Extempore package: https://github.com/benswift/extempore-sublime

If you want to run Extempore in a terminal *inside* Atom, then you can
get the ``term`` package as well, search for it in the same window as
before.

The main way to do things in Atom is through the "command palette",
which you can bring up with ``Ctrl+Shift+P`` on Windows/Linux or
``Cmd+Shift+P`` on OSX. Type in a few letters, and you'll see the name
of all the commands the system understands starting with those letters.
It's really powerful, and it's a good first place to look when you want
to do something. All of the menu bar commands in this blog post can be
accessed through the command palette, and it's usually quicker than

Writing Extempore code
^^^^^^^^^^^^^^^^^^^^^^

#. start Atom
#. open up a terminal *outside* Atom (e.g. ``Terminal`` on OS X or
   ``cmd.exe`` on Windows) or a ``term`` *inside* Atom (e.g.
   ``Packages > Term > New Right Pane Terminal``)
#. in the terminal, run ``extempore`` (just type ``extempore`` and hit
   ``return``) and you should see it print out some info about your
   system, then just sit there waiting for input
#. open (``File > Open``) an existing Extempore file such as
   ``examples/core/fmsynth.xtm``, or create and save new file with a
   ``.xtm`` extension
#. connect to the running Extempore process with the ``Extempore connect``
   command---either with the keyboard shortcut ``Alt+O`` or the
   menu bar (``Packages > Extempore > Connect``), the default
   ``host:port`` combination of ``localhost:7099`` should be fine

Then, to evaluate Extempore code, move your cursor onto (or highlight)
the code you want to evaluate and hit ``Alt+S`` to evaluate the code.
Change the code and re-evaluate it by hitting ``Alt+S`` again---the code
is live, so you can change and re-evaluate things without having to
re-start Extempore.

If you *do* want to restart the Extempore process, just ``ctrl+c`` in
the terminal where ``extempore`` is running to kill it, then start it up
again.


Emacs
-----

Installation
^^^^^^^^^^^^

``extempore-mode`` is available from `MELPA`_ - just ``M-x
package-install RET extempore-mode RET`` and you're done.

If you don't want to get it from MELPA, just `download the file`_ and
``(package-install-file "/path/to/extempore-mode.el")``

.. _MELPA: http://melpa.org/
.. _download the file: https://github.com/extemporelang/extempore-emacs-mode/blob/master/extempore-mode.el

Writing Extempore code
^^^^^^^^^^^^^^^^^^^^^^

#. start Emacs (if it isn't running already)
#. open up an Emacs buffer with an Extempore file (``extempore-mode``
   should be loaded automatically when Emacs sees the ``.xtm`` file
   extension, assuming you added code above to your ``.emacs``)
#. call ``M-x switch-to-extempore`` (``C-c C-z``), and if Extempore
   isn't already running you'll can add (optional) command line args for
   Extempore here, such as which audio device to use (e.g.
   ``./extempore --device 2``)
#. connect to the running Extempore process: ``C-c C-j`` (this needs to
   be done for *every* ``.xtm`` buffer)

Then, to evaluate Extempore code, use either

-  evaluate enclosing s-expression: ``C-c C-c`` or ``C-M-x``
-  evaluate region: ``C-c C-r``
-  evaluate whole buffer: ``C-c C-b``

To restart the Extempore process, just ``C-c C-c`` in the
``*extempore*`` buffer where ``extempore`` is running to kill it, then
start it up again with ``M-x switch-to-extempore`` (``C-c C-z``).


Sublime Text 2
--------------

`Sublime Text 2`_ (ST2) is a cross-platform text editor which runs on OS
X, Linux and Windows. It's the 'spiritual successor' to Textmate, and it
has some cool features, a large variety of plugins for different
languages and tasks, and a great community.

I won't cover all the basics of ST2 here, because the `(unofficial)
documentation`_ does that pretty well. Think of this post as a 'cheat
sheet', for dipping back into to refresh your memory when you just can't
remember how to do something off the top of your head.

.. _Sublime Text 2: http://www.sublimetext.com
.. _(unofficial) documentation: http://docs.sublimetext.info/en/latest/

Installing ST2
^^^^^^^^^^^^^^

Installing ST2 is a piece of cake---there are binaries for all platforms
on the `download`_ page.

You'll also need the `ST2 Extempore plugin`_, which provides syntax
highlighting and some commands for connecting to and working with a
running Extempore process. To install the plugin, download the `plugin
files`_ (or clone the repo) and unzip them into your `ST2 packages
directory`_ (put it in the top level, not in the ``User`` subdirectory).

.. _download: http://www.sublimetext.com/2
.. _ST2 Extempore plugin: https://github.com/benswift/extempore-sublime
.. _plugin files: https://github.com/benswift/extempore-sublime/zipball/master
.. _ST2 packages directory: http://docs.sublimetext.info/en/latest/basic_concepts.html#the-packages-directory

Writing Extempore code
^^^^^^^^^^^^^^^^^^^^^^

#. start ST2
#. open up your favourite shell (e.g. Terminal on OS X or cmd.exe on
   Windows)
#. ``cd`` into your Extempore directory and run ``extempore``, e.g.
   ``./extempore``
#. open up an ST2 buffer with an Extempore file (the Extempore plugin
   should be loaded automatically when ST2 sees the ``.xtm`` file
   extension)
#. connect to the running Extempore process with the ``extempore connect``
   command, which you can call through the command palette
   (``Ctrl+Shift+P`` on Windows/Linux or ``Cmd+Shift+P`` on OSX) or
   the menu bar (``Tools > Extempore > Connect``)

Then, to evaluate Extempore code, highlight the code you want to
evaluate and hit ``evaluate region`` (which by default is mapped to
``ctrl+e``).

To restart the Extempore process, just ``ctrl+c`` in the terminal where
``extempore`` is running to kill it, then start it up again.


Known issues
^^^^^^^^^^^^

The syntax highlighting currently doesn't cover a few edge cases---so if
you end up tinkering with ``Extempore.JSON-tmLanguage`` to fix anything
then I'd love it if you submitted a patch.

Also, ``extempore_evaluate`` currently requires **highlighting** the
code to evaluate. It would be nice if it would eval the top-level
s-expression if no region was highlighted. This is hopefully not too
tricky to add if you know a bit about how ST2 works.


Vim
---

Extempore's `vim`_ plugin can be found in the ``extras/`` subdirectory.
The plugin uses python to get the job done, so it won't work in ``vi``.
To install it, you can either ``:source`` it each time, or put it in
your vim plugin directory (probably a better option if you're going to
be using it often).

.. _vim: http://www.vim.org/

The ``extempore.vim`` plugin file has a full list of the commands up the
top, but the most important ones (and their default keybindings) are:

-  ``ExtemporeOpenConnection`` (``<Leader>o``) connect this vim session
   to a running Extempore process
-  ``ExtemporeCloseConnection`` (``<Leader>O``) close the connection
-  ``ExtemporeSendEnclosingBlock`` (``<Leader>w``) send the current
   (i.e. where the cursor is) definition to Extempore
-  ``ExtemporeSendSelection`` (``<Leader>s``) send the current selection
   to Extempore
-  ``ExtemporeSendEntireFile`` (``<Leader>a``) send the current file to
   Extempore

Remember to have your terminal (where Extempore is running) somewhere
you can see it, since Extempore's ``stdout`` will show up there (and not
in vim).

Writing Extempore code
^^^^^^^^^^^^^^^^^^^^^^

#. start vim
#. open up your favourite shell (e.g. Terminal on OS X or cmd.exe on
   Windows)
#. ``cd`` into your Extempore directory and run ``extempore``, e.g.
   ``./extempore --device 2``
#. in vim, create a new file or open an existing one (e.g. from the
   ``examples/`` subdirectory) and ``:source`` the extempore plugin
   (which is located by default in ``extras/extempore.vim``)
#. connect to the running Extempore process with
   ``:ExtemporeSendEnclosingBlock`` (or the ``<Leader>w`` keybinding)

Then, to evaluate Extempore code, position the cursor in (or highlight)
the code you want to evaluate and use the
``ExtemporeSendEnclosingBlock`` command (which by default is mapped to
``<Leader>w``).

To restart the Extempore process, just ``ctrl+c`` in the shell where
``extempore`` is running to kill it, then start it up again (you'll have
to reconnect vim to this new Extempore process).


Known issues
^^^^^^^^^^^^

The vim mode doesn't yet support multiple connections or user-specified
host/port, but pull requests are welcome!

A big thankyou to Tim Mellor and others (including Garett Shulman) who
have contributed to the vim plugin.

Text editors
============

Emacs
-----

.. note:: This was once a blog post---corrections/improvements
          welcome.

Learning `Emacs`_ can be pretty daunting, and you don't *have* to use
Emacs to use Extempore (see the `Sublime Text 2 plugin`_, for instance).
Still, if you're willing to invest the time, Emacs is a powerful and
flexible editing environment, and hacking Extempore code in Emacs can
actually be pretty fun once you get on top of it :)

I won't cover all the basics of Emacs here, because there are lots of
other places which do that pretty well. If you've never used Emacs
before, I suggest you go and read the `beginner's guide to Emacs`_,
which explains some of the basic concepts and terminology. Emacs also
has a built-in tutorial, which you can bring up with ``C-h t``.

Think of this post as more as a 'cheat sheet', for dipping back into to
refresh your memory when you just can't remember how to do something off
the top of your head.

If you're already an Emacs user
-------------------------------

**Update Nov 2015** ``extempore-mode`` is now in MELPA. So just
``M-x package-install RET extempore-mode RET`` and you're done.

If you don't want to get it from MELPA, just `download the file`_ and
``(package-install-file "/path/to/extempore-mode.el")``

Working with Extempore code
---------------------------

These are the steps youll need to take to start hacking on a piece of
Extempore code in Emacs:

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

Then, to `evaluate`_ Extempore code, use either

-  evaluate enclosing s-expression: ``C-c C-c`` or ``C-M-x``
-  evaluate region: ``C-c C-r``
-  evaluate whole buffer: ``C-c C-b``

To restart the Extempore process, just ``C-c C-c`` in the
``*extempore*`` buffer where ``extempore`` is running to kill it, then
start it up again with ``M-x switch-to-extempore`` (``C-c C-z``).

For more detail on how to 'program' in Extempore, have a look at this
post on `interacting with the Extempore compiler`_.

If you're new to Emacs
----------------------

The first thing you'll need to do is install Emacs if it isn't already
installed on your system. You can check if Emacs is installed (and which
version) with

.. code-block:: bash

    emacs --version

The Extempore Emacs mode (plugins are called 'modes' in Emacs) requires
Emacs version 24, which has been the current stable release since about
June 2012. If for some reason you're stuck on Emacs 23, you can use the
Extempore minor mode (``extras/extempore-minor.el``).

**OS X**

Binaries are available from `emacsformacosx.com`_. In general, the
default one (i.e. big 'Download' button on that page) is probably the
best option if you're unsure.

If you're a `Homebrew`_ user, then you can also get Emacs with

.. code-block:: bash

    brew install --cocoa emacs

**Linux**

On Ubuntu, you can get Emacs version 24 with

.. code-block:: bash

    sudo apt-get install emacs24

Other distros will probably have a similar package.

**Windows**

The `Official GNU Windows binaries`_ are probably as good as any. Scroll
down to the bottom of that page for the most recent versions.

Also, if you're running Extempore in an Emacs ``shell`` buffer on
Windows you'll need to pass the ``--term ansi`` option when starting
Extempore.

A .emacs to get you started
---------------------------

In Emacs, user configuration code goes into a file called ``.emacs`` in
your home directory. This includes settings about colour themes,
installed packages, keybindings, and whatever else you need to set Emacs
up just the way you like. Emacs will loads this file automatically on
start-up.

As a new Emacs user, you won't have a ``.emacs`` yet, but if you want a
bit of help getting started you can use the one in the ``extras``
directory. To use it to get started, there are two things to do:

#. copy ``extras/.emacs`` into your home directory
#. change the ``extempore-path`` variable to point to your Extempore
   source directory.

After step 1, when you start Emacs it will automatically open up the
``.emacs`` file at the place where ``extempore-path`` is defined. You
can then change it to suit your setup (you only have to do this once)
and you're good to go. Also, don't worry if Emacs takes a while to load
after you do this. On the first run-through it'll need to install some
packages, but after that subsequent start-ups shouldn't take too long.

Genral Emacs notes
------------------

Emacs is powered by a programming language called Emacs Lisp (elisp for
short). In Emacs, everything you do—every key you press, every shortcut
you invoke—is actually calling an elisp function. You can think of Emacs
as a (very specialised) elisp interpreter, on top of which sits millions
of lines (30+ years worth) of elisp code for editing files, interacting
with the OS, and doing all sorts of weird and wacky things.

In Emacs documentation, you'll often see something like ``M-x
load-theme`` (pronounced *meta x, load theme*). To trigger this command,
press the **meta** key (which will probably be ``alt`` or ``option`` on
a modern keyboard) and the ``x`` key at the same time, then (releasing
both those keys) type in the function name ``load-theme``. The elisp
function ``load-theme`` will be called, and you'll see a prompt in your
echo area which says ``Load custom theme``, and you can specify (by
name) the name of the colour theme you want to load.

Shortcut keys, too, are ultimately just triggering elisp functions. Even
pressing the letter ``e`` on the keyboard in typing actually calls a
function called ``self-insert-command`` to put the ``e`` into the buffer
you're typing in. And ``M-x`` is just a way of calling these elisp
functions by name.

The main reason to bring this up is that in some ways Emacs is not
dissimilar to Extempore. You (as a programmer) are interacting with a
running interpreter, giving commands which are evaluated, and the state
of the world is updated in response to these commands. Of course, in
lots of was Emacs and Extempore are very different, but it might be
helpful in terms of thinking about how the whole thing works.

Emacs cheat sheet
-----------------

*A note on Emacs keyboard shortcuts:* with a shortcut like ``C-x C-f``,
press ``C-x`` and *then* ``C-f``, so that the ``x`` is released before
the ``f`` is pressed (although the ``ctrl`` key *may be* held down the
whole time). In contrast, with a key sequence like ``C-M-x``, press the
``ctrl``, ``meta`` and ``x`` keys simultaneously.

File navigation
^^^^^^^^^^^^^^^

-  open file: ``C-x C-f``
-  save file: ``C-x C-s``
-  switch to buffer: ``C-x b``, then the buffer's name
-  split window horizontally: ``C-x 2``
-  split window vertically: ``C-x 3``
-  jump to other window (in split window setup): ``C-x o``

Cursor movement
---------------

In most Emacs situations, you can use *either* the regular arrow keys to
navigate, or the default Emacs navigation commands:

-  ``C-f``: forward one character
-  ``C-b``: backward one character
-  ``C-n``: forward one line
-  ``C-p``: backward one line

There are also lots of other ways to move around, including (but not
limited to)

-  beginning of line: ``C-a``
-  end of line of line: ``C-e``
-  search forward: ``C-s``
-  search backward: ``C-r``
-  set/unset mark (for highlighting): ``C-<space>``

Mastering Emacs has a great post on `effective editing`_.

Editing
-------

-  kill (cut): ``C-w``
-  copy: ``M-w``
-  yank (paste): ``C-y``, then ``M-y`` to cycle through previous kills
-  kill rest of line: ``C-k``

Getting out of (Emacs) trouble
------------------------------

-  cancel: ``C-g`` (if you get into trouble)
-  help (on a *function*): ``C-h f``, then function name
-  help (on a *variable*): ``C-h v``, then variable name
-  info: ``C-h i``, then browse through the menus

For further reading, I can recommend the `Emacs reading guide`_ at
`masteringemacs.org`_.

.. _Emacs: http://www.gnu.org/software/emacs/
.. _Sublime Text 2 plugin: https://github.com/benswift/extempore-sublime
.. _beginner's guide to Emacs: http://www.masteringemacs.org/articles/2010/10/04/beginners-guide-to-emacs/
.. _download the file: https://github.com/extemporelang/extempore-emacs-mode/blob/master/extempore-mode.el
.. _evaluate: 2012-09-26-interacting-with-the-extempore-compiler.org
.. _interacting with the Extempore compiler: 2012-09-26-interacting-with-the-extempore-compiler.org
.. _emacsformacosx.com: http://emacsformacosx.com
.. _Homebrew: http://mxcl.github.com/homebrew/
.. _Official GNU Windows binaries: http://ftp.gnu.org/gnu/emacs/windows/
.. _effective editing: http://www.masteringemacs.org/reading-guide/
.. _Emacs reading guide: http://www.masteringemacs.org/reading-guide/
.. _masteringemacs.org: http://masteringemacs.org

Extempore Atom cheat sheet
--------------------------

`Atom`_ is a cross-platform text editor which runs on OS X, Linux and
Windows. It was originally created by GitHub, but it's open source a
large (and growing) variety of packages for different languages and
tasks and a great community.

I won't cover all the basics of Atom here, because the `documentation`_
does that pretty well. Think of this post as a 'cheat sheet', for
dipping back into to refresh your memory when you just can't remember
how to do something off the top of your head.

Installing Atom
---------------

Installing Atom is a piece of cake—go to the `Atom homepage`_ and hit
the big red "Download" button near the top of the page. When it's
finished downloading, click on the downloaded file and follow the
instructions which show up on the screen.

Once you've got Atom up and running, you need to install the `Extempore
package`_—this tells the Atom editor how to deal with Extempore code.
You can do this through the menu bar: ``Packages > Settings View >
Install Packages/Themes``, then search for "extempore" and click install
on the ``Extempore-Atom`` package. Alternatively, you can install it at
a terminal with ``apm install extempore-atom``. Once that's done, you're
ready to go.

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

Hacking on Extempore code in Atom
---------------------------------

These are the steps you'll need to take to start hacking on a piece of
Extempore code in Atom:

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
   command—either with the keyboard shortcut ``Alt+O`` or the
   menu bar (``Packages > Extempore > Connect``), the default
   ``host:port`` combination of ``localhost:7099`` should be fine

Then, to evaluate Extempore code, move your cursor onto (or highlight)
the code you want to evaluate and hit ``Alt+S`` to evaluate the code.
Change the code and re-evaluate it by hitting ``Alt+S`` again—the code
is live, so you can change and re-evaluate things without having to
re-start Extempore.

If you *do* want to restart the Extempore process, just ``ctrl+c`` in
the terminal where ``extempore`` is running to kill it, then start it up
again.

For more detail on how to 'program' in Extempore, have a look at this
post on `interacting with the Extempore compiler`_.

.. _Atom: https://atom.io/
.. _documentation: http://docs.sublimetext.info/en/latest/
.. _Atom homepage: https://atom.io/
.. _Extempore package: https://github.com/benswift/extempore-sublime
.. _interacting with the Extempore compiler: 2012-09-26-interacting-with-the-extempore-compiler.org


Extempore & Sublime Text 2
--------------------------

`Sublime Text 2`_ (ST2) is a cross-platform text editor which runs on OS
X, Linux and Windows. It's the 'spiritual successor' to Textmate, and it
has some cool features, a large variety of plugins for different
languages and tasks, and a great community.

I won't cover all the basics of ST2 here, because the `(unofficial)
documentation`_ does that pretty well. Think of this post as a 'cheat
sheet', for dipping back into to refresh your memory when you just can't
remember how to do something off the top of your head.

Installing ST2
--------------

Installing ST2 is a piece of cake—there are binaries for all platforms
on the `download`_ page.

You'll also need the `ST2 Extempore plugin`_, which provides syntax
highlighting and some commands for connecting to and working with a
running Extempore process. To install the plugin, download the `plugin
files`_ (or clone the repo) and unzip them into your `ST2 packages
directory`_ (put it in the top level, not in the ``User`` subdirectory).

Hacking on Extempore code in ST2
--------------------------------

These are the steps youll need to take to start hacking on a piece of
Extempore code in ST2:

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
``ctrl+e``, but you can remap it to some other keyinding by editing the
).

To restart the Extempore process, just ``ctrl+c`` in the terminal where
``extempore`` is running to kill it, then start it up again.

For more detail on how to 'program' in Extempore, have a look at this
post on `interacting with the Extempore compiler`_.

Known issues with the Extempore ST2 plugin
------------------------------------------

The syntax highlighting currently doesn't cover a few edge cases—so if
you end up tinkering with ``Extempore.JSON-tmLanguage`` to fix anything
then I'd love it if you submitted a patch.

Also, ``extempore_evaluate`` currently requires **highlighting** the
code to evaluate. It would be nice if it would eval the top-level
s-expression if no region was highlighted. This is hopefully not too
tricky to add if you know a bit about how ST2 works.

.. _Sublime Text 2: http://www.sublimetext.com
.. _(unofficial) documentation: http://docs.sublimetext.info/en/latest/
.. _download: http://www.sublimetext.com/2
.. _ST2 Extempore plugin: https://github.com/benswift/extempore-sublime
.. _plugin files: https://github.com/benswift/extempore-sublime/zipball/master
.. _ST2 packages directory: http://docs.sublimetext.info/en/latest/basic_concepts.html#the-packages-directory
.. _interacting with the Extempore compiler: 2012-09-26-interacting-with-the-extempore-compiler.org

*** CEPL-SHADERTOY
This is a work-in-progress project to create an environment similar to shadertoy in common lisp.

CEPL allows shaders to be written in a Lisp-like DSL!

** Quickstart:

Clone the repo to a quicklisp-visible directory.

From your Repl, execute the following:

```Lisp
(ql:quickload :cepl-shadertoy)
(in-package :cepl-shadertoy)
(cepl:repl 700 394)
(run-loop)

You should see some funky stuff in the CEPL output window.

** Notes
* I usually split my Emacs to have two side-by-side windows, with the REPL in the right one.  I position the CEPL output window in the upper-right-corner of the screen, and set it to be always on top with the window manager menu.  I then load files into the left Emacs window.
* You may need to hit <ENTER> in your REPL to get a prompt!

** Exploring shaders
Load `learn.lisp` into Emacs.  The file is not meant to be compiled as a whole - read it and compile individual shaders as you go using C-c C-c.

This will get you started with some information on how to make shaders in CEPL using Varjo, the Lisp-to-GLSL compiler.

** Status
24 Oct 2016 - Basic shaders work (ones that require no video or audio or mouse support.





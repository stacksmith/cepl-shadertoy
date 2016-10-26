# CEPL-SHADERTOY
(work-in-progress) With CEPL-SHADERTOY you can create fragment shaders in a Lisp-like DSL (see [CEPL](https://github.com/cbaggers/cepl) and [Varjo](https://github.com/cbaggers/varjo)).

A fragment shader is a GPU functions that is called once for every pixel.  You can see examples [on shadertoy](https://www.shadertoy.com).  Here you can do it in Lisp.

![screenshot](Screenshot.png?raw=true)

### PLEASE SUBMIT YOUR SHADERS
To report bugs or submit shaders, please open an issue.

## Quickstart:

Clone the repo to a quicklisp-visible directory.

If you are new to [CEPL](https://github.com/cbaggers/cepl), please read the instructions there.  Generally, you must have a working OpenGL environment.  Usually loading the drivers will work.

From your REPL, execute the following:

```Lisp
(ql:quickload :cepl-shadertoy)
(in-package :cepl-shadertoy)
(cepl:repl 700 394)
(run-loop)
```
You should see some funky stuff in the CEPL output window.

## Notes
* I usually split my Emacs to have two side-by-side windows, with the REPL in the right one.  I position the CEPL output window in the upper-right-corner of the screen, and set it to be always on top with the window manager menu.  I then load files into the left Emacs window.
* You may need to hit <ENTER> in your REPL to get a prompt!

## Exploring shaders
Load `learn.lisp` into Emacs.  The file is not meant to be compiled as a whole - read it and compile individual shaders as you go using C-c C-c.

This will get you started with some information on how to make shaders in CEPL using Varjo, the Lisp-to-GLSL compiler.

** Status
24 Oct 2016 - Basic shaders work (ones that require no video or audio or mouse support.





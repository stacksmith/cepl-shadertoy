*** CEPL-SHADERTOY
This is a work-in-progress project to create an environment similar to shadertoy in Common Lisp.

CEPL allows shaders to be written in a Lisp-like DSL!

![screenshot](Screenshot.png?raw=true)
```lisp
(def-frag
  (let (((z :vec2) (/ (* 1.15
			 (- (* (s~ gl-frag-coord :xy) 2.0)
			    (s~ iResolution :xy)))
		      (y iResolution)))
	(an  (- (* (cos( + (v! 0.0 1.5708)
			   (v! (* 0.1 iGlobalTime) (* 0.1 iGlobalTime))))
		   0.51)
		(* (cos( +  (v! 0.0 1.5708)
			    (v! (* 0.2 iGlobalTime) (* 0.2 iGlobalTime))))
		   0.25)))
	(f 1e20))
    (for (i 0) (< i 120) (++ i)
	 (setf z (+ (v! (- (* (x z) (x z))
			   (* (y z) (y z)))
			(* 2.0 (x z) (y z)))
		    an)
	       f (min f (dot z z))))
    (setf f (+ 1.0 (/ (log f) 16)))
    (v! f (* f f) (* f f f) 1.0)))
```

** Quickstart:

Clone the repo to a quicklisp-visible directory.

If you are new to [CEPL](https://github.com/cbaggers/cepl), please read the instructions there.  Generally, you must have a working OpenGL environment.  Usually loading the drivers will work.

From your REPL, execute the following:

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





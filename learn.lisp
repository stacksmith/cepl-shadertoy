;;;; learn.lisp
;;;
;;; This is a simple tutorial for creating fragment shaders with CEPL and VARJO,
;;; based on https://www.shadertoy.com/view/Md23DV shadertoy tutorial.
;;;
;;; A secondary goal of this project is to shake down CEPL for bugs...  If you
;;; find some, please report.
;;
;; (ql:quickload :cepl-shadertoy)(in-package cepl-shadertoy)(cepl:repl 700 394)
;;
(in-package #:cepl-shadertoy)

DO NOT COMPILE!  This file is meant to be read...
;;------------------------------------------------------------------------------
;; 0.  Getting started.
;;
;; The code in cepl-frag-learn sets up a simple demo screen for us to use.
;; We will be changing the frag shader by recompiling it with C-c C-c to
;; demonstrate the various aspects of fragment shaders as well as CEPL and
;; VARJO.
;; 
;;------------------------------------------------------------------------------
;; 1.  Blank screen.
;;
;; The fragment shader is called on a per-pixel basis.  The expected output of
;; the shader is a Vec4, containing RGBA components of the pixel.  Varjo expects
;; the shader to return a value.  Here is how we can construct a simple shader.
;; Place the cursor on the frag function below, and compile with C-c C-c.  The
;; output window should go white.
(def-frag 
  (v! 1 1 1 0)) ;; v! is a function in rtg-math package.

;; Note that def-frag is a convenience macro in cepl-shadertoy.lisp.  It is
;; there only to make the code less cluttered.  You could just do this
;; (defun-g frag ((tex :vec2) &uniform (iGlobalTime :float) (iResolution :vec3))
;;   (v4! 1 1 1 0))
;; But while you are my guest in this package, you are encouraged to use
;; def-frag.  I am still messing around with the parameters - this way your
;; shaders will work without any changes in the future.
;;
;; Varjo is lenient in accepting types, and will accept a vector of wrong size;
;; the data, however, will be unpredictable:
(def-frag 
  (v! 1 0 ))
;;
;; This may give you a hint about how your GPU assigns code to shader units. My
;; guess is that the blocks you see indicate the size of an area assigned to a
;; hardware unit - the same uninitialized value is reused, creating a blocky
;; appearance.
;;
;; However, you can construct a vector of the right size with v4!, even if
;; there are not enough parameters.  This is identical to (v! 1.0 1.0 1.0 0.0).
(def-frag 
  (v4! 1 ))
;;------------------------------------------------------------------------------
;; Try to change the screen to different colors.  You can use decimal fractions
;; 
(def-frag
  (v! 0.7 0 0.5 1))
;;
;;#############################################################################
;;
;; Interlude - dealing with crashes
;;
;; Since we are working in an interactive environment, and the system is live
;; while we are swapping shaders, we are likely to encounter some problems.
;;
;; Certain kinds of crashes (but not all) will cause CEPL to halt the
;; application loop, and your recompiled shaders will no longer affect the
;; output screen.  An easy way to see if CEPL is running is to move something
;; in front of the window, and see if it's redrawing.  If the window is set
;; to always-on-top (as mine is), I find it easy to switch workspaces back and
;; forth.
;;
;; If the application shut down, just restart it with
(run-loop)
;; You can always stop it with
(stop-loop)
;;
;; Very occasionally, you will need to also restart the pipeline:
(def-g-> prog-1 () vert frag)
;;
;; To see what's currently in the running pipeline, execute this in the REPL:
;; (pull-g 'prog-1)
;;
;; If all else fails, you can always M-x slime-restart-inferior-lisp and start
;; over with a
;; (ql:quickload :cepl-shadertoy)(in-package cepl-shadertoy)(cepl:repl 700 394)
;;
;;#############################################################################
;;
;; Fragment shaders in CEPL can be lispy:
;;
(def-frag
  (let ((q (v! 0.2 0.3)))
    (v! q 0.1 1)))
;;
;; Can you guess the resultant vector values?  If not, execute the same let
;; expression on the CPU side in your REPL, or compile the next expression:
(print (let ((q (v! 0.2 0.3)))
	    (v! q 0.1 1)))
;;------------------------------------------------------------------------------
;; gl-frag-coord
;;
;; frag is executed for every pixel in the screen.  To find out the coordinates
;; of the pixel you are rendering, in window coordinates, use gl-frag-coord.

(def-frag
   (let ((c1 (v! 0.986 0.571 0.898 0))
	 (c2 (v! 0.537 0.741 0.408 0)))
     (if (> 100 (x gl-frag-coord)) ;left side is different color!
	 c1
	 c2)))
;;------------------------------------------------------------------------------
;; A vertical line can be expressed by monitoring the x coordiate range in
;; either pixel form, or with r, which is normalized between 0.0 and 1.0
;;
;;
(def-frag
  (let ((c1 (v! 0.816 0.471 0.698 0))
	(background (v! 0 0 0 0)))
    (if (and (< .53 (x tex)) ;conveniently expresses x between 0.0 and 1.0
	     (> .55 (x tex)))
	c1
	background)))
;;
;;------------------------------------------------------------------------------
;; A horizontal line, similarly, requires monitoring the y coordinate range. 
;;
;;
(def-frag
  (let ((c1 (v! 0.216 0.471 0.698 0))
	(c2 (v! 1.00  0.32  0.298 0))
	(c3 (v! 0.867 0.910 0.247 0))
	(background (v! 0 0 0 0))
	(pixel (v! 0 0 0 0)))
    ;; vertical line
    (setf pixel
	  (if (< (abs (- (x tex) 0.4))
		 0.01)
	      c1
	      background))
    ;; horizontal line
    (setf pixel
	  (if (< (abs (- (y tex) 0.6))
		 0.01)
	      c2
	      pixel))
    pixel))

;;
;; Look at learn-more for some cooler shaders.
;;
void mainImage(out vec4 o, vec2 i)
{
  float f=length(i)/iResolution.y;
  o=vec4((1.+cos(1e3*mix(1.,sin(iGlobalTime),.3)*f*f))/2.);
}
(def-frag
  (let ((f (/ (length gl-frag-coord) (y iResolution))))
    (v4! (/ (+ 1.0 (cos (* 1e3 f f (mix 2.0 (sin iGlobalTime) .3)) ))
	    2.0))))
;;----------------------------------------------------------------------------
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 p = (-iResolution.xy + 2.0*fragCoord)/iResolution.y;
    // angle of each pixel to the center of the screen
    float a = atan(p.y,p.x);
    // modified distance metric
    float r = pow( pow(p.x*p.x,4.0) + pow(p.y*p.y,4.0), 1.0/8.0 );
    // index texture by (animated inverse) radious and angle
    vec2 uv = vec2( 1.0/r + 0.2*iGlobalTime, a );
    // pattern: cosines
    float f = cos(12.0*uv.x)*cos(6.0*uv.y);
    // color fetch: palette
    vec3 col = 0.5 + 0.5*sin( 3.1416*f + vec3(0.0,0.5,1.0) );
     // lighting: darken at the center    
    col = col*r;
     // output: pixel color
    fragColor = vec4( col, 1.0 );

;; does not work as it should...
;; TODO: decompile, fix!
(def-frag
  (let* ((p (- (s~ (* 2.0 gl-frag-coord) :xy)
	       (s~ iResolution :xy)))
	 ((a :float) (atan (x p) (y p))) ;; angle to the center
	 ((r :float) (pow (+ (pow (* (x p) (x p)) 4.0)
		       (pow (* (y p) (y p)) 4.0)) (/ 1.0 8.0) ))
	 ((uv :vec2) (v2! (+ (/ 1.0 r) (* 0.2 iGlobalTime)) a))
	 (f (* (cos (* 12.0 (x uv)))
	       (cos (*  6.0 (y uv)))))
	 ((col :vec3) (+  (* 0.5 (sin (+ (v3! (* f 3.1416))
					 (v3! 0 0.5 1.0))))
			  (v3! 0.5))))
    (setf col (* col r))
    (v4! (x col) (y col) (z col) 1.0)))

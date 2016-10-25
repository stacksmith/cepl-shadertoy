;;;; cepl-frag-learn.lisp
;;;
;;; This is a simple tutorial for creating fragment shaders with CEPL and VARJO,
;;; based on https://www.shadertoy.com/view/Md23DV shadertoy tutorial.  As such,
;;; it is of limited use, as only the fragment shader is being used and then in
;;; a very particular setting.  However, it may be useful to you.
;;;
;;; The shaders are in a separate file called "vert-shaders.lisp"; simply open
;;; that file in Emacs and compile whichever shader you want using C-c C-k. Note
;;; that some shaders are in multiple functions, so make sure to compile all the
;;; required functions.
;;;
;;; A secondary goal of this project is to shake down CEPL for bugs...  If you
;;; find some, please report.
;; (ql:quickload :cepl-shadertoy)(in-package cepl-shadertoy)(cepl:repl 700 394)

(in-package #:cepl-shadertoy)

;;; "cepl-frag-learn" goes here. Hacks and glory await!

;; *quad* is a list compatible with g-pt format (vec-3 pos and a vec-2 tex)
(defparameter *running* nil)
(defparameter *quad* (list (list (v! -1    1  0) (v! 0.0 0.0))
			   (list (v! -1   -1  0) (v! 0.0 1.0))
			   (list (v!  1   -1  0) (v! 1.0 1.0))
			   (list (v! -1    1  0) (v! 0.0 0.0))
			   (list (v!  1   -1  0) (v! 1.0 1.0))
			   (list (v!  1    1  0) (v! 1.0 0.0))))

(defparameter *vert-array* nil)
(defparameter *vert-stream* nil)

(defparameter *iGlobalTime* nil) ;will get from sdl
(defparameter *iResolution* (v! 700 394 1.0))
(defun-g vert ((vert g-pt))
  (values (v! (pos vert) 1.0) 
          (tex vert)))

(defun-g frag ((tex :vec2) &uniform (iGlobalTime :float) (iResolution :vec3))
 (let ((color  (sin (/ (* (x gl-frag-coord) (y gl-frag-coord))
			(float (mod  iGlobalTime 30))))))
   (v4! color color color 1 )))
   
(def-g-> prog-1 ()
     vert frag)


(defmacro def-frag (&body body)
  `(defun-g frag ((tex :vec2) &uniform (iGlobalTime :float) (iResolution :vec3))
     ,@body))

(defun step-demo ()
  
  (step-host)        ;; Advance the host environment frame

  (update-repl-link) ;; Keep the REPL responsive while running

  (clear)            ;; Clear the drawing buffer

  (setf *iGlobalTime* (float (/ (sdl2:get-ticks) 1000)))
  (map-g #'prog-1 *vert-stream*
	 :iGlobalTime *iGlobalTime*
	 :iResolution *iResolution*
	 )
  (swap))            ;; Display newly rendered buffer 


(defun run-loop ()
  (with-viewport (make-viewport (list (truncate (x *iResolution*))
				      (truncate (y *iResolution*))))
    (setf *running* t
	  ;; Create a gpu array from our Lisp vertex data
	  *vert-array* (make-gpu-array *quad* :dimensions 6
				       :element-type 'g-pt)
	  ;; Create a GPU datastream
	  *vert-stream* (make-buffer-stream *vert-array*))
    ;; continue rendering frames until *running* is set to nil
    (loop :while (and  *running*
		       (not (shutting-down-p))) :do
       (continuable (step-demo)))))

(defun stop-loop ()
  (setf *running* nil))



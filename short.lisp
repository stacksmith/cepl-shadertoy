;;; short shaders
;;;
(in-package #:cepl-shadertoy)

https://www.shadertoy.com/view/4tfXW8
void mainImage( out vec4 f, vec2 u ) {
//  f += sin( dot(u+=u,u) - max(u.x,u.y) );             // 70 chars
    f = sin( dot(u+=u,u) - max(u.x,u.y) - 4.*iDate.wwww); // anim: + 11 chars
}
;; Notes: It would be nice to not create vectors for + and -
(def-frag
  (let ((u (*  gl-frag-coord 2)))
    (sin (- (v4! (dot u u))
	    (v4! (max (x u) (y u)))
	    (* 4.0 (s~ *iDate* :wwww))))))

;;------------------------------------------------------------------------------
https://www.shadertoy.com/view/XtXXzH
void mainImage(out vec4 o,vec2 i){o=sin(i.xyxy*iDate.wwww);}

(def-frag
  (sin (* .1 (s~ gl-frag-coord :xyxy)
	  (s~ *iDate* :wwww))))

;;------------------------------------------------------------------------------

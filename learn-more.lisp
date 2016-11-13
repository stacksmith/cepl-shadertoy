;;;; wiki-fract.lisp
;;;
;;; Examples from wikip page https://en.wikibooks.org/wiki/Fractals/shadertoy
;;;
;; (ql:quickload :cepl-shadertoy)(in-package cepl-shadertoy)(cepl:repl 700 394)
;;
(in-package #:cepl-shadertoy)

DO NOT COMPILE THE FILE!  C-c C-c the individual fragment shaders one at a time.
Keep cepl:repl open and running.
(def-g-> prog-1 ()
     vert frag)

;;------------------------------------------------------------------------------
;; First Image Shader
;;
;;This shader computes the same color ( red = 1.0,0.0,0.0 ) for every pixel
;;
;;// static image 
;;void mainImage( out vec4 fragColor, in vec2 fragCoord )
;;{
;;	// color = r,g,b,a 
;;	// every value is in range [0.0, 1.0]
;;	fragColor = vec4(1.0,0.0,0.0,1.0);
;;}
;;
(def-frag
  (v! 1 0 0 1))


;;------------------------------------------------------------------------------
;; Diagonal gradient
;;
;;void mainImage( out vec4 fragColor, in vec2 fragCoord )
;;{
;;    float r = fragCoord.x / iResolution.x;
;;    float g = fragCoord.y / iResolution.y;
;;    float b = fragCoord.x / iResolution.y;
;;    fragColor = vec4(r,g,b,1.0);
;;}
(def-frag
  (let ((r (/ (x gl-frag-coord) (x *iResolution*)))
	(g (/ (y gl-frag-coord) (y *iResolution*)))
	(b (/ (x gl-frag-coord) (y *iResolution*))))
	(v! r g b 1.0)))
;;------------------------------------------------------------------------------
;; Cross
;;(π pi)
;;
;;void mainImage( out vec4 fragColor, in vec2 fragCoord ) {			
;;	const float PI = 3.14159265359;
;;	float thicknessH = 0.01;
;;	float thicknessV = 0.01;
;;	
;;	float y = fragCoord.y / iResolution.y;
;;	float x = fragCoord.x / iResolution.x;
;;
;;	float diffY = abs(0.5 - y);
;;	float diffX = abs(0.5 - x);
;;	
;;	if ( diffY < thicknessH || diffX < thicknessV) {
;;		fragColor = vec4(1.0, 0.0, 0.0, 1.0 );
;;	} else {
;;		fragColor = vec4(0.0, 0.0, 0.0, 1.0 );
;;	}
;;}
(def-frag
  (let* ((thickness-h 0.01)
	 (thickness-v 0.01)
	 (y (/ (y gl-frag-coord) (y *iResolution*))) 
	 (x (/ (x gl-frag-coord) (x *iResolution*)))
	 (diff-x (abs (- 0.5 x)))
	 (diff-y (abs (- 0.5 y))))
    (cond ;or is malfunctioning as of 10/23/16
      ((< diff-y thickness-h) (v! 0 0 1 1))
      ((< diff-x thickness-v) (v! 0 0 1 1))
      (t (v! 1 1 1 1)))))

;; red rect
;;void mainImage( out vec4 fragColor, in vec2 fragCoord )
;;{   
;;    float x = fragCoord.x/iResolution.x; // 0.0 < x < 1.0
;;    float y = fragCoord.y/iResolution.y; // 
;;    vec2 center = vec2(0.5, 0.5); // center of the image
;;    
;;    fragColor = vec4(0.0,1.0,0.0,1.0); // green = background
;;    if( abs(center.x - x) < 0.2 && abs(center.y - y) < 0.2)
;;      {fragColor = vec4(1.0,0.0,0.0,1.0);} // red rectangle in center of image 
;;}
;;
(def-frag
  (let* ((x (/ (x gl-frag-coord) (x *iResolution*)))
	 (y (/ (y gl-frag-coord) (y *iResolution*)))
	 (center (v! 0.5 0.5)))
      (if (and  (< (abs (- (x center) x)) 0.2)
		(< (abs (- (y center) y)) 0.2))
	  (v! 1 0 0 1)
	  (v! 0 1 0 1))))


;; Square
;;
;;void mainImage( out vec4 fragColor, in vec2 fragCoord )
;;{   float ratio = iResolution.x / iResolution.y; // aspect ratio of the window : https://github.com/mattdesl/lwjgl-basics/wiki/ShaderLesson3
;;    float x = ratio*fragCoord.x/iResolution.x; // 0.0 < x < 1.0*ratio
;;    float y = fragCoord.y/iResolution.y; //  0.0 < y < 1.0
;;    vec2 center = vec2(ratio*0.5, 0.5); // center of the image 
;; 
;;    fragColor = vec4(0.0,1.0,0.0,1.0); // green = background
;;    if( abs(center.x - x) < 0.2 && abs(center.y - y) < 0.2) {fragColor = vec4(1.0,0.0;;,0.0,1.0);} // red rectangle in center of image 
;;}
(def-frag
  (let* ((ratio (/ (x *iResolution*) (y *iResolution*)))
	(x (/ (* ratio (x gl-frag-coord))
	      (x *iResolution*)))
	(y (/ (y gl-frag-coord)
	      (y *iResolution*)))
	(center (v! 0.5 0.5)))
    (if (and  (< (abs (- (x center) x)) 0.2)
		(< (abs (- (y center) y)) 0.2))
	  (v! 1 0 0 1)
	  (v! 0 1 0 1))))
;; Circle
;; 
;;// https://www.shadertoy.com/view/Mdf3Df
;;// circle using pixel coordinate
;;void mainImage( out vec4 fragColor, in vec2 fragCoord )
;;{
;;    // the center of the texture
;;    vec2 center = vec2(iResolution.x/2.0,iResolution.y/2.0); 
;;    // current pixel location
;;    vec2 loc = fragCoord.xy;
;;    // how far we are from the center
;;    float radius=length(loc-center);
;;    if (radius<100.0)
;;        fragColor = vec4(1,0,0,1); // if we are within our circle, paint it red
;;    else
;;        fragColor = vec4(0,0,0,1); // black background
;;}
(def-frag
  (let* ((center (v! (/ (x *iResolution*) 2)
		     (/ (y *iResolution*) 2)))
	 (loc (v! (x gl-frag-coord)
		  (y gl-frag-coord)))
	 (radius (length (- loc center))))
    (if (< radius 100)
	(v! 1 0 0 0)
	(v! 0 0 0 0))))

(def-frag
  (let* ((center (/ (s~ *iResolution* :xy) 2)) ;divide vector by scalar
	 (loc (s~ gl-frag-coord :xy))
	 (radius (length (- loc center))))
    (v4! (if (< radius 100) 0 1 ) 0)))

(def-frag
  (let ((k 1.0))
    (v4! k 0)))


;; Julia set from https://www.shadertoy.com/view/XsS3Rm
#|
const int max_iterations = 255;
vec2 complex_square( vec2 v)
(
 v.x * v.x - v.y * v.y,
 v.x * v.y * 2.0
 )					;
}
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
	vec2 uv = fragCoord.xy - iResolution.xy * 0.5;
	uv *= 2.5 / min( iResolution.x, iResolution.y );
	
// Julia
	vec2 c = vec2( 0.285, 0.01 );
	vec2 v = uv;
	float scale = 0.01;
	
	int count = max_iterations;
	
	for ( int i = 0 ; i < max_iterations; i++ ) {
		v = c + complex_square( v );
		if ( dot( v, v ) > 4.0 ) {
			count = i;
			break;
		}
	}
	fragColor = vec4( float( count ) * scale );
}
|#

(defun-g complex-square ((v :vec2))
    (v! (- (* (x v) (x v))
	   (* (y v) (y v)))
	(* (x v) (y v) 2.0)))

(def-frag
  (let* ((uv (* (- (s~ gl-frag-coord :xy)
		   (* (s~ *iResolution* :xy) 0.5))
		(/ 2.5
		   (min (x *iResolution*)
			(y *iResolution*)))))
	 ((c :vec2) (v! 0.285 0.01))
	 ((v :vec2) uv)
	 (scale 0.01)
	 (max-iterations 255)
	 (count max-iterations))
    (for (i 0) (< i max-iterations) (++ i)
	 (setf v (+ c (complex-square v)))
	 (if (> (dot v v) 4.0)
	     (progn
	       (setf count i)
	       (setf i max-iterations)
	      ) ;; for lack of break
	     0))
    (v! (* count scale)(* count scale)(* count scale) 1.0)))
#|
// Created by inigo quilez - iq/2014
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License
// line 13: pixel coordinates	
// line 15: c travels around the main cardiod c(t) = Â½e^it - Â¼e^i2t
// line 20: z = zÂ² + c		
// line 21: trap orbit
// line 24: remap	
// line 26: color	

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
	vec2 z = 1.15*(-iResolution.xy+2.0*fragCoord.xy)/iResolution.y;

	vec2 an = 0.51*cos( vec2(0.0,1.5708) + 0.1*iGlobalTime ) - 0.25*cos( vec2(0.0,1.5708) + 0.2*iGlobalTime );

	float f = 1e20;
	for( int i=0; i<128; i++ ) 
	{
		z = vec2( z.x*z.x-z.y*z.y, 2.0*z.x*z.y ) + an;
		f = min( f, dot(z,z) );
	}
	
	f = 1.0+log(f)/16.0;

	fragColor = vec4(f,f*f,f*f*f,1.0);
}
|#
;; original translation
(def-frag
  (let* (((z :vec2) (/ (* 1.15 (- (* (s~ gl-frag-coord :xy) 2.0)
				  (s~ *iResolution* :xy)))
		       (y *iResolution*)))
	 (vtemp (v2! 0.0 1.5708))
	 (vtime (v2! (* .05 *iGlobalTime*)))
	 (an  (- (* 0.51 (cos (+ vtemp vtime)))
		 (* 0.25 (cos (+ vtemp vtime vtime)))))
	 (f 1e20))
    (for (i 0.0) (< i 120.0) (++ i)
	 (let ((xz (x z)) (yz (y z)))
	   (setf z (+ an (v! (- (* xz xz) (* yz yz))
			     (* 2.0 xz yz)))
		 f (min f (dot z z)))))
    (setf f (+ 1.0 (/ (log f) 16)))
    (v! f (* f f) (* f f f ) 1.0)))

;; slightly better color selection
(def-frag
  (let* (((z :vec2) (/ (* 1.15 (- (* (s~ gl-frag-coord :xy) 2.0)
				  (s~ *iResolution* :xy)))
		       (y *iResolution*)))
	 (vtime (v2! (* .05 *iGlobalTime*)))
	 (vtemp (+ (v2! 0.0 1.5708) vtime))
	 (an  (- (* 0.51 (cos vtemp))
		 (* 0.25 (cos (+ vtemp vtime)))))
	 (f 1e20))
    (for (i 0) (< i 120) (++ i)
	 (let ((xz (x z)) (yz (y z)))
	   (setf z (+ an (v! (- (* xz xz) (* yz yz))
			     (* 2.0 xz yz)))
		 f (min f (dot z z)))))
    (setf f (- (/ (log f) 8)))
    (v! f  (* f f) (* f f f ) 1.0)))


#|
https://www.shadertoy.com/view/4dlGW7   
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
	vec2 uv = fragCoord.xy / sin(0.5 * iGlobalTime) / iResolution.xy;
	vec2 p = sin(2.0 * uv - 1.0);
	float gt = sin(3.0 * iGlobalTime);
	p *= abs(sin(p.x * gt) * tan(iGlobalTime));
	p /= abs(sin(p.y / sin(4.0 * iGlobalTime)));
	p *= abs(sin(p.x * cos(iGlobalTime)));
	p /= tan(abs(sin(p + gt)*3.0));
	
	fragColor = vec4(0.3,0.3,0.3,1.0);
	float scanline1 = abs(sin(10.0 * iGlobalTime + p.y * 14.0));
	scanline1 = abs(sin(scanline1));
	float scanline2 = abs(sin(11.0 * iGlobalTime + p.x * 10.0));
	scanline2 = abs(sin(scanline2));
	fragColor.y = scanline1 * 0.3;
	fragColor.y += scanline2 * 0.2;
}
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    float color = sin((fragCoord.x * fragCoord.y) / (iGlobalTime * 3.0));
    fragColor = vec4(color);
}

|#
(def-frag
  (let ((color  (sin (/ (* (x gl-frag-coord) (y gl-frag-coord))
			(float (mod  *iGlobalTime* 30))))))
    (v4! color color color 1 )) 
)

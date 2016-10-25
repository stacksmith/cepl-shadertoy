;;;
;; (ql:quickload :cepl-shadertoy)(in-package cepl-shadertoy)(cepl:repl 700 394)
;;
(in-package #:cepl-shadertoy)

;;
;; https://www.shadertoy.com/view/Ms2XDW
void mainImage( out vec4 fragColor, in vec2 fragCoord ) {
	vec2 uv = ( fragCoord.xy / iResolution.xy ) * iResolution.y/iResolution.x;
	float t = iGlobalTime * 0.1;
	// Rotate the uv coordinates.
	float x1 = uv.x;
	float y1 = uv.y;
	uv.x = x1*cos(t) - y1*sin(t);
	uv.y = x1*sin(t) + y1*cos(t);
	// Render a line pattern along the x axis
	float zoomFactor = 50.0+(sin(iGlobalTime * 1.0)*0.50 + 0.50)*5.0;
	float x = sin(uv.x * zoomFactor);
	float y = sin(uv.y * zoomFactor);
	float c = sin( cos(tan( cos(x) + sin(y))) + tan( cos(x) + sin(y) ) );
	fragColor = vec4( c*uv.x, c*uv.y, c*sin(uv.x*uv.y+iGlobalTime), 1.0 );
}


(def-frag
  (let* (((uv :vec2)  (* (/ (s~ gl-frag-coord :xy) (s~ iResolution :xy))
			    (/ (y iResolution) (x iResolution))))
	 (tt (* iGlobalTime 0.1))
	 (x1 (x uv))
	 (y1 (y uv)))
    (setf (x uv) (- (* x1 (cos tt))
		    (* y1 (sin tt)))
	  (y uv) (+ (* x1 (sin tt)
		       y1 (cos tt))))
    (let* (((zoomFactor :float) (+ 50 (* 5(+ 0.5 (* 0.5 (sin iGlobalTime))))))
	   (x (sin (* (x uv) zoomFactor)))
	   (y (sin (* (y uv) zoomFactor)))
	   (t1 (tan (+ (cos x) (sin y))))
	   (c (sin (+ t1 (cos t1)))))
      
      (v! (* c (x uv))
	  (* c (y uv))
	  (* c (sin (+ (* (x uv) (y uv))
		       iGlobalTime)))
	  1.0))))
;;yz²

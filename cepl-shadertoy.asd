;;;; cepl-frag-learn.asd

(asdf:defsystem #:cepl-shadertoy
  :description "A CL shadertoy-like environment for fragment shaders"
  :author "fpgasm@apple2.x10.mx"
  :license "BSD"
  :depends-on (#:cepl
               #:temporal-functions
               #:cepl.sdl2
               #:swank
               #:livesupport
               #:cepl.skitter.sdl2
               #:cepl.devil)
  :serial t
  :components ((:file "package")
               (:file "cepl-shadertoy")
	       ))


(asdf:defsystem #:guitar-fx
  :description "Guitar + live-electronics setup."
  :author "Nuno Trocado"
  :license  "MIT"
  :version "1.0"
  :serial t
  :depends-on (#:alexandria #:kdtree-jk #:cl-collider #:simple-inferiors)
  ;; :depends-on (#:alexandria #:sc-vst #:kdtree-jk)
  :components ((:file "package")
	       (:file "util")
	       (:file "setup")
	       (:file "guitar-fx")
	       (:file "corpus")
	       (:file "gui")
	       ;; vst works, but for the moment I'd rather use the amp modeller as a standalone
	       ;; (:file "vst")
	       ))

;;;; Guitar-FX.asd

(asdf:defsystem #:Guitar-fx
  :description "Guitar + live electronics setup."
  :author "Nuno Trocado"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:sc-setup #:sc-vst #:kdtree-jk)
  :components ((:file "package")
               (:file "guitar-fx")
	       (:file "corpus")
	       ;; (:file "vst")
	       ))

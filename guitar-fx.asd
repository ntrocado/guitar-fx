;;;; Guitar-FX.asd

(asdf:defsystem #:Guitar-fx
  :description "Some effects that I use with guitar."
  :author "Nuno Trocado"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:sc-setup)
  :components ((:file "package")
               (:file "guitar-fx")))

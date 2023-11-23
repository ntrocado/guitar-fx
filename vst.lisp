(in-package #:guitar-fx)

(defparameter *vst-group* (make-group :pos :before))

(defsynth s-gear ()
  (out.ar 0 (sc-vst:vst-plugin.ar (sound-in.ar 1) 2 :s-gear)))

(defparameter *s-gear* (sc-vst:vst-controller (synth 's-gear :to *vst-group*) :s-gear "S-Gear3_x64"))

(defun s-gear-open-editor ()
  (sc-vst:editor *s-gear*))

(sc-osc:add-osc-responder
    *osc*
    "/s-gear"
    (lambda (&rest param)
      (declare (ignore param))
      (s-gear-open-editor)))

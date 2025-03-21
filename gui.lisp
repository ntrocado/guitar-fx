(in-package #:guitar-fx)

;; Edit this to set the path expressly
(defvar *open-stage-control-program* nil
  "Path to Open-Stage-Control executable.")

;; Try to find Open-Stage-Control if its path is not specified
(unless *open-stage-control-program*
  (setf *open-stage-control-program*
	
	#+windows
	(a:when-let ((dir (find-if (a:compose (a:curry #'search "open-stage-control")
					      #'namestring)
				   (uiop:subdirectories (uiop:getenv-pathname "ProgramFiles"))
				   :from-end t)))
	  (merge-pathnames "open-stage-control.exe" dir))

	#+darwin
	(uiop:file-exists-p
	 #p"/Applications/open-stage-control.app/Contents/MacOS/open-stage-control")

	#+linux
	(handler-case
	    (uiop:run-program "which open-stage-control" :output :line)
	  (t (c)
	    nil))))

(defun run-gui ()
  (flet ((source-relative (file)
	   (uiop:native-namestring
	    (merge-pathnames file (asdf:system-source-directory "guitar-fx")))))
    (bt:make-thread
     (lambda ()
       (simple-inferiors:run (uiop:native-namestring *open-stage-control-program*)
			     (list
			      #+windows "--"
			      "--send" "localhost:8000"
			      "--osc-port" "8088"
			      "--load" (source-relative #p"gui/guitar-fx.json")
			      "--custom-module" (source-relative #p"gui/midi-osc.js")
			      "--midi" "fcb:0,1"
			      "--no-qrcode")
			     :output t
			     :copier :line))
     :name "guitar-fx gui")))

#-slynk (run-gui)

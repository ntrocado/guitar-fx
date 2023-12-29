(in-package #:guitar-fx)

(setf *s*
      (make-external-server
       "localhost"
       :server-options (make-server-options :num-input-bus 8
					    :num-output-bus 10
					    :realtime-mem-size 131072
					    :block-size 16
					    :hardware-samplerate 44100
					    :device "ASIO : ASIO Fireface")
       :port 4444))

(defun start-sc ()
  (unless (boot-p *s*)
    (loop :initially (server-boot *s*)
	  :with start := (get-universal-time)
	  :do (sleep 1)
	  :until (or (boot-p *s*)
		     ;; 10 second timeout
		     (> (get-universal-time) (+ start 10))))))

(start-sc)

(in-package #:guitar-fx)


;;; prevent sleep while the server is on

(defvar *caffeinate-proc* nil)

(defun start-caffeinate ()
  (unless *caffeinate-proc*
    ;; -d: prevent display sleep; -s: prevent system sleep; -i: prevent idle sleep
    (setf *caffeinate-proc* 
          (uiop:launch-program '("caffeinate" "-dimsu")))))

(defun stop-caffeinate ()
  (when *caffeinate-proc*
    (uiop:terminate-process *caffeinate-proc*)
    (setf *caffeinate-proc* nil)))

;; Register hooks in cl-collider
(pushnew 'start-caffeinate sc:*server-boot-hooks*)
(pushnew 'stop-caffeinate sc:*server-quit-hooks*)

;;; add server kill and reboot to cl-collider

;; Patch sc-program-run to ignore non-zero exit status (e.g. exit code 138 / SIGBUS on macOS
;; when unloading plugins on quit, or 143 when killed), preventing UIOP from signaling
;; SUBPROCESS-ERROR in the background "scsynth" thread.
#+sbcl
(sb-ext:without-package-locks
  (setf (fdefinition 'sc::sc-program-run)
        (lambda (program options)
          (ignore-errors
            (uiop:run-program (cons program options)
                              :output *standard-output*
                              :error-output *debug-io*
                              :ignore-error-status t)))))
#-sbcl
(setf (fdefinition 'sc::sc-program-run)
      (lambda (program options)
        (ignore-errors
          (uiop:run-program (cons program options)
                            :output *standard-output*
                            :error-output *debug-io*
                            :ignore-error-status t))))

;; Fix cl-collider bug where build-server-options mutates hardware-buffer-size to NIL,
;; causing a TYPE-ERROR (= NIL 0) on subsequent server boots.
#+sbcl
(sb-ext:without-package-locks
  (unless (get 'sc::build-server-options :patched)
    (let ((orig-build (fdefinition 'sc::build-server-options)))
      (setf (fdefinition 'sc::build-server-options)
            (lambda (server-options)
              (when (and server-options
                         (null (sc::server-options-hardware-buffer-size server-options)))
                (setf (sc::server-options-hardware-buffer-size server-options) 0))
              (funcall orig-build server-options)))
      (setf (get 'sc::build-server-options :patched) t))))
#-sbcl
(unless (get 'sc::build-server-options :patched)
  (let ((orig-build (fdefinition 'sc::build-server-options)))
    (setf (fdefinition 'sc::build-server-options)
          (lambda (server-options)
            (when (and server-options
                       (null (sc::server-options-hardware-buffer-size server-options)))
              (setf (sc::server-options-hardware-buffer-size server-options) 0))
            (funcall orig-build server-options)))
    (setf (get 'sc::build-server-options :patched) t)))

(defun server-kill (&optional (server *s*))
  "Forcefully kill a hung scsynth and reset cl-collider state."
  (when server
    (let ((*s* server))  ;; needed because tempo-clock-stop references *s*
      (let ((exited nil))
        (when (boot-p server)
          (ignore-errors (send-message server "/quit"))
          (setf exited (sc::thread-wait-with-timeout
                        (lambda () (not (boot-p server))) 1000)))
        (unless exited
          (uiop:run-program '("killall" "-9" "scsynth") :ignore-error-status t)
          (sleep 0.5)  ;; give process time to die
          (setf (boot-p server) nil)))
      (when (and (sc::sc-thread server)
                 (find-package :bt)
                 (uiop:symbol-call :bt :thread-alive-p (sc::sc-thread server)))
        (ignore-errors (uiop:symbol-call :bt :destroy-thread (sc::sc-thread server))))
      (when (and server (sc::server-options server))
        (when (null (sc::server-options-hardware-buffer-size (sc::server-options server)))
          (setf (sc::server-options-hardware-buffer-size (sc::server-options server)) 0)))
      (setf (boot-p server) nil)
      (setf (sc::node-watcher server) nil
            (sc::sample-rate server) nil)
      (ignore-errors (sc::sched-stop (sc::scheduler server)))
      (ignore-errors (sc::tempo-clock-stop (sc::tempo-clock server)))
      (ignore-errors (sc::cleanup-server server))
      (setf (sc::sc-thread server) nil)
      (dolist (f *server-quit-hooks*)
        (ignore-errors (funcall f)))))
  :killed)

(defun server-reboot (&optional (server *s*))
  "Kill hung server and reboot fresh."
  (server-kill server)
  (when (and server (sc::server-options server))
    (when (null (sc::server-options-hardware-buffer-size (sc::server-options server)))
      (setf (sc::server-options-hardware-buffer-size (sc::server-options server)) 0)))
  (sleep 0.5)
  (server-boot server))

;;;

(setf *s*
      (make-external-server
       "localhost"
       :server-options (make-server-options :num-input-bus 10
					    :num-output-bus 10
					    :realtime-mem-size 131072
					    :block-size 64
					    :hardware-samplerate 44100
					    :device "Babyface (23509327)")
       :port 4444
       :just-connect-p nil))

(defun start-sc ()
  (unless (boot-p *s*)
    (loop :initially (server-boot *s*)
	  :with start := (get-universal-time)
	  :do (sleep 1)
	  :until (or (boot-p *s*)
		     ;; 10 second timeout
		     (> (get-universal-time) (+ start 10))))))

(start-sc)


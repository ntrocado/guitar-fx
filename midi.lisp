(in-package #:guitar-fx)

(midi:initialize)

(defvar *midi-source* (or (midi:find-source "SoftStep Share") 0))

(midi:set-midi-callback *midi-source* :note-on
			(lambda (chan note vel)
			  (format t "channel: ~d notenum: ~d velocity: ~d~%" chan note vel)
			  (case note
			    (1 (loop-rec-start))
			    (2 (zigzag-on))
			    (3 (onsets-on))
			    (4 (random-fm-on))
			    (6 (sin-follow-on))
			    (7 (vocal-bass-on))
			    (8 (crazy-pad-on))
			    (9 (feedback-fm-on))
			    (10 (panic))
			    (11 (loop-clear))
			    (16 (new-sound))
			    (22 (loop-replace-start))
			    (107 (glitch-loop-play-start)))))

(midi:set-midi-callback *midi-source* :note-off
			(lambda (chan note vel)
			  (format t "channel: ~d notenum: ~d velocity: ~d~%" chan note vel)
			  (case note
			    (1 (loop-rec-stop))
			    (2 (zigzag-off))
			    (3 (onsets-off))
			    (4 (random-fm-off))
			    (6 (sin-follow-off))
			    (7 (vocal-bass-off))
			    (8 (crazy-pad-off))
			    (9 (feedback-fm-off))
			    (22 (loop-replace-stop))
			    (107 (glitch-loop-play-stop)))))

(midi:set-midi-callback *midi-source* :cc
			(lambda (chan cc val)
			  (declare (ignore chan))
			  (case cc
			    (20 (if (is-playing-p *ctrl-node*)
				    (ctrl *ctrl-node* :val (/ val 127))
				    (setf *ctrl-node* (synth 'ctrl-bus))))
			    (117 (ctrl *glitch-buf-play-node* :speed (/ val 127))))))

(in-package :guitar-fx)

(defparameter *input-mic* 0)
(defparameter *input-pre* 1)
(defparameter *input-post* 6)
(defparameter *output-bus* 4)

(defparameter *nodes* (make-hash-table))

(defparameter *osc* (sc-osc:osc-device nil nil :local-port 8000))

(defun osc-toggle (resource start-fun stop-fun toggle-fun &key (device *osc*) (verbose t))
  (sc-osc:add-osc-responder
   device
   resource
   (lambda (&rest param)
     (destructuring-bind (state) param
       (cond ((zerop state) (funcall stop-fun))
	     ((= state 1) (funcall start-fun))
	     ((= state 2) (funcall toggle-fun)))
       (when verbose
	 (format t "~a: ~a~%" resource state)
	 (finish-output))))))

(defmacro make-toggle (name &key (gate-p nil) (pos :head))
  (let ((start-fun (intern (concatenate 'string (symbol-name name) "-ON")))
	(stop-fun (intern (concatenate 'string (symbol-name name) "-OFF")))
	(toggle-fun (intern (concatenate 'string (symbol-name name) "-TOGGLE"))))

    (a:with-gensyms (node)
      `(progn
	 (defun ,start-fun ()
	   (setf (gethash ',name *nodes*) (synth ',name :pos ,pos)))

	 (defun ,stop-fun ()
	   (a:if-let (,node (gethash ',name *nodes*))
	     (if ,gate-p
		 ;; use gate for synths that have a gate arg to release and free themselves
		 (ctrl ,node :gate 0)
		 (free ,node))
	     (warn "Node not found."))
	   (setf (gethash ',name *nodes*) nil))

	 (defun ,toggle-fun ()
	   (if (gethash ',name *nodes*)
	       (,stop-fun)
	       (,start-fun)))

	 (osc-toggle (concatenate 'string "/" (symbol-name ',name))
		     #',start-fun
		     #',stop-fun
		     #',toggle-fun)))))

;;; OSC transmit

(defparameter *osc-send-port* 8088)

(defun sequence->osc-string (seq)
  (format nil "[~{~,4f~^, ~}]" (coerce seq 'list)))

(defun send-osc-message (target message &key (port *osc-send-port*))
  (sc-osc:send-message (sc-osc:osc-device #(127 0 0 1) port)
		       target
		       (if (typep message 'sequence)
			   (sequence->osc-string message)
			   message)))

;;;;

(defparameter *aux-group* (make-group :pos :before))

(defparameter *in-bus* (list :pre (bus-audio) :post (bus-audio)))

(defsynth in-bus-pre ((in *input-pre*))
  (out.ar (getf *in-bus* :pre) (sound-in.ar in)))

(defsynth in-bus-post ((in *input-post*))
  (out.ar (getf *in-bus* :post) (sound-in.ar in)))

(defparameter *in-bus-pre-node* (synth 'in-bus-pre :to *aux-group*))
(defparameter *in-bus-post-node* (synth 'in-bus-post :to *aux-group*))

(sc-osc:add-osc-responder
 *osc*
 "/input-pre"
 (lambda (&rest param)
   (ctrl *in-bus-pre-node* :in (first param))))

(sc-osc:add-osc-responder
 *osc*
 "/input-post"
 (lambda (&rest param)
   (ctrl *in-bus-post-node* :in (first param))))

;;;;

(defparameter *ctrl-bus* (bus-control))

(defsynth ctrl-bus ((val 0) (bus *ctrl-bus*))
  (out.kr bus val))

(defparameter *ctrl-node* (synth 'ctrl-bus :to *aux-group*))

(defsynth sin-test ((in *ctrl-bus*))
  (out.ar *output-bus* (sin-osc.ar (+ 300 (* 200 (in.kr in))) 0 0.1)))

(sc-osc:add-osc-responder
 *osc*
 "/ctrl"
 (lambda (&rest param)
   (destructuring-bind (state) param
     (if (is-playing-p *ctrl-node*)
	 (ctrl *ctrl-node* :val state)
	 (setf *ctrl-node* (synth 'ctrl-bus)))
     (format t "Control: ~a~%" state)
     (finish-output))))

;;;;

(sc-osc:add-osc-responder
   *osc*
   "/STOP"
   (lambda (&rest param)
     (declare (ignore param))
     (stop)
     (setf *nodes* (make-hash-table))
     (print "Stop")
     (finish-output)))

;;;;

(defsynth feedback-fm ((amp .2))
  (let* ((control (in.kr *ctrl-bus*))
	 (in-freq (first (tartini.kr (in.ar (getf *in-bus* :pre)))))
	 (max-speed (range (lf-noise1.kr 1) 2 8))
	 (freq (+ in-freq
		  (* (lf-noise0.kr (lin-lin.kr control 0 1 1 max-speed))
		     (lin-exp.kr control 0 1 0.001 80))))
	 (mod (sin-osc-fb.ar
	       (* freq
		  (range (lf-pulse.kr (range (lf-noise1.kr '(.7 1.1))
					     .3 1.2))
			 (range (lf-pulse.kr '(.8 1.3))
				'(.5 1) '(2 3))
			 (range (lf-pulse.kr '(.2 .6))
				3.9 4.1)))))
	 (car (sin-osc-fb.ar (+ freq mod)
			     (range (lf-noise1.kr (lf-noise1.kr '(.4 .23 .69) .5 .8))
				    (* .2 pi) (* .9 pi)))))
    (out.ar *output-bus*
	    (splay.ar (freeverb.ar (decimator.ar (rlpf.ar car (range (lf-noise0.kr (lin-lin.kr control 0 1 .5 2)) 1600 5000))
						 (+ (range (lin-lin.kr control 0 1 44100 8000))
						    (lf-noise1.kr (lin-lin.kr control 0 1 .2 .7)
								  (lin-exp.kr control 0 1 0.001 1500)))
						 (range (lin-lin.kr control 0 1 24 6))))
		      (range (lf-noise1.kr 1) .1 1)
		      (* (env-follow.ar (in.ar (getf *in-bus* :pre)))
			 ;; (range (lf-noise1.kr .1) .1 1)
			 amp)))))

(make-toggle feedback-fm)

;;;;

(defsynth random-fm ((amp .7))
  (let ((input (in.ar (getf *in-bus* :pre))))
    (out.ar *output-bus*
	    (pan2.ar (* (+ (sin-osc.ar (tartini.kr input) 0 .2)
			   (sin-osc.ar (* (amplitude.kr input
							:mul 2000
							:add 1)
					  (range (lf-noise0.kr 1)
						 20 10000))
				       0 .2))
			(max 0 (- .15 (amplitude.kr input :mul 10)))
			.35 amp)))))

(make-toggle random-fm)

;;;;

(defsynth onsets ((notes '(43 44 50 51 53 55 62 63 65 67 68 76 78)) (amp .7))
  (let* ((in (in.ar (getf *in-bus* :pre)))
	 (trig (coyote.kr in 0.2 0.2 0.01 0.8 0.05 0.1)))
    (out.ar *output-bus* (pan2.ar (leak-dc.ar
				   (* (env-gen.ar (perc 0.0001
							(demand.kr trig 0 (d-rand '(.05 .07 .09 .2 1)
										  +inf+))
							(demand.kr trig 0 (d-white .07 .7))
							-4)
						  :gate trig)
				      (+ (resonz.ar (lf-saw.ar (demand.kr trig 0
									  (d-rand (mapcar #'midicps notes)
										  +inf+))
							       0 0.5)
						    (+ 100 (demand.kr trig 0 (d-white 0 1800))))
					 (bpf.ar (white-noise.ar 0.3)
						 (+ 100 (demand.kr trig 1 (d-white 200 2000)))
						 (+ .1 (demand.kr trig 1 (d-white .1 2)))
						 ))
				      amp))
				  (lf-noise1.kr 5 2 -1)))))

(make-toggle onsets)

;;;;

(defparameter *buf* (buffer-alloc (* (slot-value (server-options *s*) 'sc::hardware-samplerate)
				     4)))

(defsynth zigzag ()
  (record-buf.ar (in.ar (getf *in-bus* :post)) *buf*)
  (out.ar *output-bus* 
	  (splay.ar (buf-rd.ar 1 *buf* (phasor.ar 1
						  (lf-noise1.kr .2 1 (mapcar (lambda (x) (* x (+ (in.kr *ctrl-bus*) .5) 2))
													     '(1.5 1.9 -1.5 1.1 -.7 .2)))
						  0
						  (buf-frames.ir *buf*)))
		    1 .4)))

(make-toggle zigzag)

;;;;

(defparameter *loop-buf* (buffer-alloc (* (slot-value (server-options *s*) 'sc::hardware-samplerate)
					  60)))

(defsynth loop ((buf *loop-buf*) (rec 0))
  (let* ((in (in.ar (getf *in-bus* :post)))
	 (dur (sweep.ar rec (* (sample-rate.ir) rec)))
	 (ptr (phasor.ar rec 1 0 dur))
	 (play (* (- 1 (var-lag.kr rec .5))
		  (buf-rd.ar 1 buf (wrap.ar (* ptr (+ 1 (* .05 (in.kr *ctrl-bus*)
							   (lf-noise1.kr 2 .45 .5))))
					    0 dur)))))
    
    (buf-wr.ar (x-fade2.ar (* in
			      1	;; (env-gen.kr (linen .05) :gate rec)
			      )
			   play
			   (var-lag.kr (- 1 (* 2 rec)) .03))
	       buf ptr)
    (out.ar *output-bus* (pan-az.ar 4
				    (* play
				       (env-gen.kr (asr .01 1 .7)
						   :gate (- 1 (* (delay-1.kr (< dur (* (sample-rate.ir) .25))) (- 1 rec)))
						   :act :free))
				    (lf-saw.kr (range (in.kr *ctrl-bus*) 0.1 2))))))


(defparameter *loop-node* nil)

(sc-osc:add-osc-responder
 *osc*
 "/LOOP"
 (lambda (&rest param)
   (destructuring-bind (state) param
     (if (zerop state)
	 (ctrl *loop-node* :rec 0)
	 (if (is-playing-p *loop-node*)
	     (ctrl *loop-node* :rec 1)
	     (setf *loop-node* (synth 'loop :rec 1))))
     (format t "Loop: ~a~%" state)
     (finish-output))))


;;; REC

(defparameter *rec*
  (buffer-alloc (* 4 (sc::server-options-hardware-samplerate (server-options *s*)))))

(defun update-sample-plot (buf)
  (let ((buffer-len (frames buf))
	(frames (buffer-to-array buf :channels 0)))
    (loop :with vec := (make-array 500)
	  :for i :from 0 :below buffer-len :by (ceiling (/ buffer-len 500))
	  :for j :from 0
	  :do (setf (aref vec j) (aref frames i))
	  :finally (send-osc-message "/sample_plot" vec))))

(sc-osc:add-osc-responder
    *osc*
    "/sample_redraw"
    (lambda (&rest param)
      (declare (ignore param))
      (update-sample-plot *rec*)))

(defsynth record ((buffer *rec*))
  (record-buf.ar (in.ar (getf *in-bus* :post)) buffer))

(make-toggle record)

;;; Pre-recorded samples

(defun load-sample (n &optional (buffer *rec*))
  (flet ((brc (path)
	   (buffer-read-channel path :channels 0 :bufnum (bufnum buffer))))
    (ecase n
      (0 (brc "~/OneDrive/Documents/Mnemosyne/throat.wav"))
      (1 (brc "~/OneDrive/Documents/Mnemosyne/valkyrie.wav"))
      (2 (brc "~/OneDrive/Documents/Taipal/f2.wav"))))
  (update-sample-plot buffer))

(sc-osc:add-osc-responder
    *osc*
    "/load_sample"
    (lambda (&rest param)
      (let ((value (car param)))
	(load-sample value))))

;;; Grains

(defsynth grains ((buffer *rec*) (rate 32) (pos .5) (amp 1.0) (gate 1))
  (let* ((t-rate #+mouse (mouse-y.kr 8 120 :exp)
		 #-mouse rate)
	 (dur (/ 12 t-rate))
	 (clk (impulse.kr t-rate))
	 (position (+ (* (buf-dur.kr buffer) pos)
		      (t-rand.kr 0 0.01 clk)))
	 (pan (lf-noise1.kr 10 2 -1)))
    (out.ar *output-bus* (* (tgrains.ar 4 clk buffer 1 position dur pan 0.5)
			    (env-gen.ar (asr 1) :gate gate :act :free)
			    amp))))

(sc-osc:add-osc-responder
 *osc*
 "/XY"
 (lambda (&rest param)
   (destructuring-bind (x y)
       param
     (a:when-let (node (gethash 'grains *nodes*))
       (ctrl node :pos x)
       (ctrl node :amp y)))))

(sc-osc:add-osc-responder
 *osc*
 "/grainX"
 (lambda (&rest param)
   (a:when-let (node (gethash 'grains *nodes*))
     (ctrl node :pos (first param))
     (print (first param)))))

(sc-osc:add-osc-responder
 *osc*
 "/grainY"
 (lambda (&rest param)
   (a:when-let (node (gethash 'grains *nodes*))
     (ctrl node :amp (first param)))))

(make-toggle grains :gate-p t)


;;;;

(defsynth hit ((buffer *rec*) (gate 1) (dur-ctl .5) (rate-ctl .5))
  (let* ((dur (lag.kr (lin-lin dur-ctl 0 1 .02 .31) .5))
	 (window .02)
	 (loop-trig (impulse.kr (/ 1 dur)))
	 (env (env-gen.ar (env '(0 1 0)
			       (list window (- dur window))
			       '(:lin -8))
			  :gate loop-trig))
	 (start (t-rand.kr 0
			   (buf-frames.ir buffer)
			   gate))
	 (rate (lag.kr (lin-lin rate-ctl 0 1 .9 1.1) .5))
	 (sig (play-buf.ar 1 buffer rate :trig loop-trig :start-pos start)))
    (out.ar *output-bus* (* sig env (env-gen.kr (asr 0 1 .1) :gate gate :act :free)))))

(sc-osc:add-osc-responder
 *osc*
 "/hit_xy"
 (lambda (&rest param)
   (destructuring-bind (x y)
       param
     (a:when-let (node (gethash 'hit *nodes*))
       (ctrl node :dur-ctl x)
       (ctrl node :rate-ctl y)))))

(make-toggle hit :gate-p t)


(defsynth fm ((freq 500) (m-ratio 1) (c-ratio 1) (index 1) (i-scale 5)
	      (amp .2) (atk .01) (rel 3) (c-atk 4) (c-rel -4) (pan 0)
	      (reverb 0))
  (let* ((ienv (env-gen.kr (env (list index (* index i-scale) index)
				(list atk rel)
				(list c-atk c-rel))))
	 (env (env-gen.kr (perc atk rel 1 (list c-atk c-rel))))
	 (mod (sin-osc.ar (* freq m-ratio) 0 (* freq m-ratio ienv)))
	 (car (* (sin-osc.ar (+ (* freq c-ratio)
				mod))
		 env amp))
	 (sig (apply #'freeverb2.ar (append (pan2.ar car pan)
					    (list :mix reverb)))))
    (detect-silence.ar sig 1.0e-4 :act :free)
    (out.ar *output-bus* sig)))

(sc-osc:add-osc-responder *osc* "/fm"
    (lambda (&rest param)
      (declare (ignore param))
      (synth 'fm
	     :freq (midicps 72)
	     :m-ratio (rrand .95 80)
	     :c-ratio (rrand 1.2 45)
	     :index 1.2
	     :i-scale (rrand 1.4 10)
	     :amp (rrand .03 .09)
	     :pan (rrand -.7 .7)
	     :rel (exp-rrand .015 1.2)
	     :reverb (rrand .1 .6))))


;;; Mag noise

(defsynth mag-noise ((buffer *rec*) (w1 12) (w2 10))
  (let ((window1 (expt 2 w1))
	(window2 (expt 2 w2)))
    (out.ar *output-bus* (pan-az.ar 4
				    (ifft.ar (pv-mag-noise (fft (local-buf (list window1 window2))
								(play-buf.ar 1 buffer .75 :loop 1))))
				    (lf-saw.kr)))))

(make-toggle mag-noise)


;;; Harmonizer

(defun progressive-harmonics (ctrl chord)
  (let ((len (truncate (lin-lin ctrl 0 1 1 (length chord)))))
    (subseq chord 0 len)))

;; 0 → 1
;; 1 → '(1/2 4/5 1 6/5 11/7 17/9 23/11)

(defsynth harmonizer ((in *output-bus*) (out *output-bus*) (ctrl *ctrl-bus*))
  (replace-out.ar out
		  (splay.ar (pitch-shift.ar (in.ar in)
					    .2
					    (mapcar (lambda (x)
						      (* x (range (in.kr ctrl)
								  .5 1)))
						    '(.68 .83 1 1.17 1.55 1.89 2.15))))))

;; (proxy :harm
;;        (replace-out.ar *output-bus*
;; 		  (splay.ar (pitch-shift.ar (in.ar *output-bus*)
;; 					    .2
;; 					    (mapcar (lambda (x)
;; 						      (* x (range (in.kr *ctrl-bus*)
;; 								  .5 1)))
;; 						    '(.68 .83 1 1.17 1.55 1.89 2.15)))))
;;        :pos :tail)
;;;;

;;;

(defsynth osc-synth (freq amp pan (curve -11) (out *output-bus*))
  (out.ar out (* (env-gen.ar (perc .01 2 1 curve) :act :free)
		 (pan2.ar (sin-osc.ar freq 0 amp)
			  pan))))

(defsynth planta ((amp .3) (gate 1) (out *output-bus*))
  (out.ar out (* (env-gen.ar (asr .001 amp 1) :gate gate :act :free)
		 (let ((trig (dust.kr (range (sin-osc.kr .1) 15 .1))))
		  (hpf.ar (sos.ar (env-gen.ar (perc) :gate trig)
				  0 2 0 (demand.kr trig 1 (d-white .45 1.6))
				  '(-0.992 -0.9995))
			  600)))))

(make-toggle planta)

(defun decay+tremolo-env (&optional (repeats 30))
  (let ((levels (append '(0 1 0)
			(loop :for l :from 0 :upto 1 :by (/ 1 repeats)
			      :append (list (lin-exp l 0 1 0.001 .9) 0))))
	(times (append '(.001 .3)
		       (loop :repeat (1+ repeats) :append (list .007 .072)))))
    (env levels times :sine)))

(defsynth glitch-osc-synth (freq amp pan (out *output-bus*))
  (out.ar out (freeverb.ar (* (env-gen.ar (decay+tremolo-env) :level-scale 1.5)
			      (pan2.ar (sin-osc.ar freq 0 amp)
				       (range (lf-tri.kr .3) -1 1)
				       pan))
			   :mix (x-line.kr .1 .7 4 :act :free))))

(defparameter *microscale* '(60 62 64 65.5 67 68.5 69.5 71))

(defun microtonal ()
  (synth 'osc-synth
	 :freq (midicps (a:random-elt
			 (loop :for octave :upto 2
			       :append (mapcar (lambda (x)
						 (+ x (* 12 octave)))
					       *microscale*))))
	 :amp (rrand .02 .13)
	 :pan (rrand -.9 .9)
	 :curve (rrand -16 -7)))

(defun microtonal-glitch ()
  (synth 'glitch-osc-synth
	 :freq (midicps (a:random-elt
			 (loop :for octave :upto 2
			       :append (mapcar (lambda (x)
						 (+ x (* 12 octave)))
					       *microscale*))))
	 :amp (rrand .1 .2)
	 :pan (rrand -.8 .8)
	 :repeats (rrand 10 20)))

(sc-osc:add-osc-responder *osc* "/microtonal"
    (lambda (&rest param)
      (declare (ignore param))
      (microtonal)))

(sc-osc:add-osc-responder *osc* "/microtonal-glitch"
    (lambda (&rest param)
      (declare (ignore param))
      (microtonal-glitch)))


;;; square wave

(defsynth square ((amp .2))
  (replace-out.ar
   *output-bus*
   (freeverb.ar
    (limiter.ar
     (* (pan2.ar (select.ar (lf-pulse.ar (range (in.kr *ctrl-bus*) .6 1.2)
					 0
					 (lin-lin.kr (in.kr *ctrl-bus*) 0 1 .95 .001))
			    (list
			     (resonz.ar (gendy1.ar 1 3 1.0 1.0 30
						   (range (sin-osc.kr .3) 50 180)
						   (sin-osc.kr .2)
						   (sin-osc.kr .1 .1)
						   12)
					600
					(range (sin-osc.kr .3) .1 10))
			     (in.ar *output-bus* 2))))
	amp)
     .95))))

(make-toggle square :pos :tail)


;;; tremelo

(defsynth tremelo ((speed 5))
  (replace-out.ar *output-bus*
		  (* (in.ar *output-bus* 2) (lf-pulse.kr speed))))

(make-toggle tremelo :pos :tail)

(sc-osc:add-osc-responder *osc* "/tremelo-speed"
    (lambda (&rest param)
      (destructuring-bind (speed)
	  param
	(print (first param))
	(ctrl (gethash 'tremelo *nodes*) :speed speed))))


(defun gui () ;; Does not work
  (uiop:launch-program "c:/Programas/Open-stage-control/open-stage-control.exe -- --send localhost:8000 --load c:/Users/trocado/OneDrive/Documents/Lisp/cl-collider/guitar-fx/guitar-fx.json --custom-module C:\Users\trocado\OneDrive\Documents\Lisp\cl-collider\guitar-fx\midi-osc.js --osc-port 8088"
		       :output *standard-output* :error-output *standard-output*))


;;; Synth stuff

(defparameter *sin-follow-buf* (buffer-alloc 2048))

(defun exp-rand (mi ma)
  (lin-exp (random 1.0) 0 1.0 mi ma))

(defun ranged-random (mi ma)
  (+ mi (random (- ma mi))))

(defmacro repeat (n &body expr)
  `(loop :repeat ,n
	 :collect ,@expr))

(defun new-sound ()
  (buffer-fill *sin-follow-buf*
	       :sine
	       (sort (copy-seq (repeat 16 (exp-rand 0.05 0.9))) #'<)
	       :frequencies (sort (copy-seq (repeat 16 (exp-rand 0.75 0))) #'>)
	       :phases (repeat 16 (random (* 2 pi)))))

(new-sound)

(defsynth sin-follow ((amp .3))
  (let* ((in-freq (first (tartini.kr (hpf.ar (in.ar (getf *in-bus* :pre))
					     60))))
	 (sig (leak-dc.ar (resonz.ar (mix  (osc.ar *sin-follow-buf*
						   (list (* .5 in-freq)
							 (* 1.01 in-freq))))
				     440))))
    (out.ar *output-bus*
	    (pan2.ar (compander.ar (tanh sig))
		     0
		     (* (env-follow.ar (in.ar (getf *in-bus* :pre)))
			(* 6 amp))))))

(make-toggle sin-follow)

(sc-osc:add-osc-responder
 *osc*
 "/sin-follow-new-sound"
 (lambda (&rest param)
   (declare (ignore param))
   (new-sound)))

(defsynth vocal-bass ((amp 1))
  (let* ((in (in.ar (getf *in-bus* :pre)))
	 (in-freq (/ (first (tartini.kr (hpf.ar (in.ar (getf *in-bus* :pre))
						60)))
		     2))
	 (snd (pulse.ar in-freq))
	 (snd (rlpf.ar snd (x-line.kr 170 8000 3)
		       .2))
	 (snd (* snd (line.kr 0 1 '(.3 .2))))
	 (snd (sum (bpf.ar snd (mapcar (lambda (x) (lin-exp (squared (normalizer.ar x))
							    0 1 100 8000))
				       (alexandria:iota 20))
			   .2)))
	 (snd (+ (rlpf.ar snd '(200 230)) (hpf.ar (* snd (dbamp -15)) 3000)))
	 (snd (tanh snd)))
    (out.ar *output-bus* (compander.ar (* (env-follow.ar in)
					  snd
					  amp)))))

(make-toggle vocal-bass)

(defsynth crazy-pad ((amp 1))
  (let* ((in (in.ar (getf *in-bus* :pre)))
	 (sig (pitch-shift.ar in
			      '(1.5 1 .1 .075 .05)
			      (list .5
				    1
				    (range (lf-noise1.ar .14) 1.5 2.1)
				    (range (lf-noise1.ar .19) 1.6 2.2)
				    2)
			      .02 .004))
	 (sig (rlpf.ar sig 700 .1))
	 (sig (freeverb.ar sig)))
    (out.ar *output-bus* (splay.ar sig 1 amp))))

(make-toggle crazy-pad)

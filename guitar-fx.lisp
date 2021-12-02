(in-package :guitar-fx)

(defparameter *input-mic* 0)
(defparameter *input-pre* 1)
(defparameter *input-post* 4)

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

(defmacro make-toggle (name &key (gate-p nil))
  (let ((start-fun (intern (concatenate 'string (symbol-name name) "-ON")))
	(stop-fun (intern (concatenate 'string (symbol-name name) "-OFF")))
	(toggle-fun (intern (concatenate 'string (symbol-name name) "-TOGGLE"))))

    (a:with-gensyms (node)
      `(progn
	 (defun ,start-fun ()
	   (setf (gethash ',name *nodes*) (synth ',name)))

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
  (out.ar 0 (sin-osc.ar (+ 300 (* 200 (in.kr in))) 0 0.1)))

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

(defsynth feedback-fm ((amp .6))
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
    (out.ar 0 (splay.ar (rlpf.ar car (range (lf-noise0.kr (lin-lin.kr control 0 1 .5 2)) 1600 5000))
			(range (lf-noise1.kr 1) .1 1)
			(* (env-follow.ar (in.ar (getf *in-bus* :pre)))
			   ;; (range (lf-noise1.kr .1) .1 1)
			   amp)))))

(make-toggle feedback-fm)

;;;;

(defsynth random-fm ()
  (let ((input (in.ar (getf *in-bus* :pre))))
    (out.ar 0
	    (pan2.ar (* (+ (sin-osc.ar (tartini.kr input) 0 .2)
			   (sin-osc.ar (* (amplitude.kr input
							:mul 2000
							:add 1)
					  (range (lf-noise0.kr 1)
						 20 10000))
				       0 .2))
			(max 0 (- .15 (amplitude.kr input :mul 10)))
			.35)))))

(make-toggle random-fm)

;;;;

(defsynth onsets ((notes '(43 44 50 51 53 55 62 63 65 67 68 76 78)))
  (let* ((in (in.ar (getf *in-bus* :pre)))
	 (trig (coyote.kr in 0.2 0.2 0.01 0.8 0.05 0.1)))
    (out.ar 0 (pan2.ar (leak-dc.ar
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
				      ))))
		       (lf-noise1.kr 5 2 -1)))))

(make-toggle onsets)

;;;;

(defparameter *buf* (buffer-alloc (* (slot-value (server-options *s*) 'sc::hardware-samplerate)
				     4)))

(defsynth zigzag ()
  (record-buf.ar (in.ar (getf *in-bus* :post)) *buf*)
  (out.ar 0 
	  (splay.ar (buf-rd.ar 1 *buf* (phasor.ar 1
						  (lf-noise1.kr .2 1 '(1.5 1.9 -1.5))
						  0
						  (buf-frames.ir *buf*))))))

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
							   (lf-noise1.kr 2 1 .5))))
					    0 dur)))))
    
    (buf-wr.ar (x-fade2.ar (* in
			      1;; (env-gen.kr (linen .05) :gate rec)
			      )
			   play
			   (var-lag.kr (- 1 (* 2 rec)) .03))
	       buf ptr)
    (out.ar 0 (pan2.ar (* play
			  (env-gen.kr (asr .01 1 .7)
				      :gate (- 1 (* (delay-1.kr (< dur (* (sample-rate.ir) .25))) (- 1 rec)))
				      :act :free))
		       (range (* (in.kr *ctrl-bus*) (lf-noise1.kr 1)) -1 1)))))


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

(defsynth record ((buffer *rec*))
  (record-buf.ar (in.ar (getf *in-bus* :post)) buffer))

(make-toggle record)

(record-on)

;;; Grains

(defsynth grains ((buffer *rec*) (rate 32) (pos .5) (amp 1.0) (gate 1))
  (let* ((t-rate #+mouse (mouse-y.kr 8 120 :exp)
		 #-mouse rate)
	 (dur (/ 12 t-rate))
	 (clk (impulse.kr t-rate))
	 (position (+ (* (buf-dur.kr buffer) pos)
		      (t-rand.kr 0 0.01 clk)))
	 (pan (lf-noise1.kr 10 2 -1)))
    (out.ar 0 (* (tgrains.ar 2 clk buffer 1 position dur pan 0.5)
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

(make-toggle grains :gate-p t)

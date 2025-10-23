(in-package #:guitar-fx)

(defsynth play-slice (buf start stop (pan 0) (amp 1))
  (out.ar 0 (pan2.ar (* (buf-rd.ar 1 buf (phasor.ar 0 (buf-rate-scale.ir buf) start stop) 0)
			(env-gen.kr (env '(0 1 1 0)
					 (list .03 (- (/ (- stop start)
							 (buf-sample-rate.ir buf))
						      .06)
					       .03))
				    :act :free)
			amp)
		     pan)))

(defstruct slice
  index
  start
  end
  avg-centroid
  max-loudness
  norm-centroid
  norm-loudness)

(defparameter *src* nil)
(defparameter *slices* nil)
(defparameter *current-slice* nil)

(defun play-slice (buf slice)
  (unless (and *current-slice* (= (slice-index slice) *current-slice*))
    (synth 'play-slice
	   :buf buf
	   :start (slice-start slice)
	   :stop (min (+ (slice-start slice) (* 2 (sr buf)))
		      (or (slice-end slice) (frames buf)))
	   :pan (rrand -.25 .25)
	   :amp (rrand .3 1.0))
    (setf *current-slice* (slice-index slice))))

(defun sequence->osc-string (seq)
  (format nil "[~{~,4f~^,~}]" (coerce seq 'list)))

(defun clear-canvas ()
  (send-osc-message "/canvasHelper" '(-1 -1)))

(defun populate-canvas (&optional (slices *slices*))
  (clear-canvas)
  (sleep .04)
  (loop :for sl :across slices
	:do (send-osc-message "/canvasHelper" (list (slice-norm-centroid sl)
						    (- 1 (slice-norm-loudness sl))))
	:do (sleep .03)))

(sc-osc:add-osc-responder *osc* "/corpus2d"
    (lambda (&rest param)
      (destructuring-bind (x y)
	  param
	(when (and (zerop x) (zerop y))
	  (setf *current-slice* nil))
	(play-slice *src* (find (nearest x y) *slices* :key #'slice-index)))))

(defun fill-kdtree (slices)
  (let ((kd (kdtree-jk:build-kdtree 2 :npoints 0)))
    (loop :for sl :across slices
	  :do (kdtree-jk:insert-2d kd
				   (slice-norm-centroid sl)
				   (slice-norm-loudness sl)
				   (slice-index sl)))
    kd))

(defparameter *kd*
  (when *slices* (fill-kdtree *slices*)))

(defun nearest (x y)
  (elt (kdtree-jk:kdresult-obj-vec
	(kdtree-jk:kd-find-k-nearest *kd*
				     (make-array 2 :element-type 'double-float
						 :initial-contents (mapcar (lambda (x)
									     (coerce x 'double-float))
									   (list x y)))
				     1))
       0))

(defun load-analysis (path)
  (destructuring-bind (src slices)
      (uiop:with-safe-io-syntax (:package :guitar-fx)
	(uiop:read-file-forms path))
    (setf *src* (buffer-read-channel (corpus-relative-pathname src) :channels 0)
	  *slices* slices
	  *kd* (fill-kdtree *slices*))
    (populate-canvas)))

(defun corpus-relative-pathname (file)
  (asdf:system-relative-pathname "guitar-fx"
				 (merge-pathnames file #p"corpus/")))

(sc-osc:add-osc-responder *osc* "/load_analysis"
    (lambda (&rest param)
      (ecase (truncate (car param))
	(1 (load-analysis (corpus-relative-pathname "example-analysis")))
	(2 (load-analysis (corpus-relative-pathname "bm-analysis")))
	(3 (load-analysis (corpus-relative-pathname "vilnius-analysis")))
	(4 (load-analysis (corpus-relative-pathname "obras-analysis")))
	(5 (load-analysis (corpus-relative-pathname "babel-analysis"))))))

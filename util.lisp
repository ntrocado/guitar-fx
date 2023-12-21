(in-package #:guitar-fx)

(defun exp-rand (&optional (rate 1.0))
  "Exponential distributed random number on [0,1)."
  (- (/ (log (- 1 (* (- 1 (exp (- rate))) (random 1.0))))
	rate)))

(defun rrand (mi ma)
  "Random number between MInimum and MAximum."
  (+ mi (random (- ma mi))))
  
(defun exp-rrand (mi ma &optional (rate 1.0))
  "Exponential distributed random number between MIminum and MAximum."
  (+ mi (* (exp-rand rate) (- ma mi))))

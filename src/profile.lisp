
(in-package :clboy)

(defstruct profiler
  (timings ())
  (average 0)
  (cur 0))

(defun update-profiler-timings (prof timing)
  (when (= (length (profiler-timings prof)) 10)
    (setf (profiler-average prof) (coerce (/ (+ (profiler-average prof) (/ (apply #'+ (profiler-timings prof)) 10)) 2) 'float)))
  (setf (profiler-timings prof)
        (cons timing
              (if (>= (length (profiler-timings prof)) 10) () (profiler-timings prof)))))

(defmacro prof-time-operation (prof &body body)
  `(let ((prof-before (get-internal-real-time)))
     ,@body
     (update-profiler-timings ,prof (- (get-internal-real-time) prof-before))))

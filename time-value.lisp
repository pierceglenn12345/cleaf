;;;; time-value.lisp

(in-package #:time-value)

(defun tvm (&key n i pv pmt fv (beg t))
  (declare (optimize (speed 3) (safety 0)))
  (labels (
	   ;; These are standard tvm equations
	   (pv (per int fut) (/ fut (expt (1+ int) per)))
	   (fv (per int pre) (* pre (expt (1+ int) per)))
	   (i  (per pre fut) (- (expt (/ fut pre) (/ 1 per)) 1))
	   (n  (int pre fut) (/ (log (/ fut pre)) (log (1+ int))))

	   ;; These are pv annuity equations
	   (pva  (per int pmt) (* pmt (/ (- 1 (expt (+ 1 int) (* -1 per))) int)))
	   (nap  (int pre pmt) (/ (log (expt (1- (/ (* pre int) pmt)) -1)) (log (1+ int))))
	   ;;iap - figure out how to find interest
	   (pmtp (per int pre) (* -1 (/ (* int pre) (1- (expt (1+ int) (* -1 per))))))

	   ;; These are fv annuity equations
	   (fva  (per int pmt) (* pmt (/ (- (expt (1+ int) per) 1) int)))
	   (naf  (int pmt fut) (/ (log (1+ (/ (* fut int) pmt))) (log (1+ int))))
	   ;;iap - figure out how to find interest
	   (pmtf (per int fut) (/ (* fut int)  (- (expt (1+ int) per) 1)))
	   )
    (cond
      ;; standard tvm calls
      ((and n i  fv) (pv n i  fv))
      ((and n i  pv) (fv n i  pv))
      ((and n pv fv) (i  n pv fv))
      ((and i pv fv) (n  i pv fv))
      
      ;; pva calls
      ((and n i  pmt beg) (pva  n i  pmt))
      ((and i pv pmt beg) (nap  i pv pmt))
      ((and n pv pmt beg) nil) ;;iap - figure out how to find interest
      ((and n i  pv  beg) (pmtp n i  pv))

      ;;fva calls
      ((and n i   pmt beg) (fva  n i   pmt))
      ((and i pmt fv  beg) (naf  i pmt fv))
      ((and n pv  pmt beg) nil) ;;iap - figure out how to find interest
      ((and n i   fv  beg) (pmtf n i   fv))

      ;; errror
      (t (error "Whoops!")))))




;;EOF

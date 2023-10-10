;;;; time-value.lisp

(in-package #:time-value)

(defun tvm (target &key n i pv pmt fv (end t))
  "This is a function that will take the correct key arguments to find the target value. the target value can be any of the key value parameter symbols."	
  (declare (optimize (speed 3) (safety 0)))
  (labels (
	   ;; Helper functions
	   (rund (v n)
	     (declare (type float v)
		      (type (integer 0) n))
	     (let ((10^-n (expt 10 (- n))))
	       (* (fround v 10^-n)
		  10^-n)))
	   
	   ;; These are standard tvm equations
	   (pv (per int fut) (/ fut (expt (1+ int) per)))
	   (fv (per int pre) (* pre (expt (1+ int) per)))
	   (i  (per pre fut) (- (expt (/ fut pre) (/ 1 per)) 1))
	   (n  (int pre fut) (/ (log (/ fut pre)) (log (1+ int))))

	   ;; These are pv annuity equations
	   (pva  (per int pmt) (* pmt (/ (- 1 (expt (+ 1 int) (* -1 per))) int)))
	   (nap  (int pre pmt) (/ (log (expt (1- (/ (* pre int) pmt)) -1)) (log (1+ int))))
	   (iap  (per pre pmt) (let ((i 0.001))
				 (labels ((recur ()
					    (cond
					      ((> (- pre (pva per i pmt)) 0) (rund i 5))
					      ((< pre (pva per i pmt)) (progn (setf i (+ i .001)) (recur)))
					      (t nil))))
				   (recur))))
	   (pmtp (per int pre) (* -1 (/ (* int pre) (1- (expt (1+ int) (* -1 per))))))

	   ;; These are pv annuity due equations
	   (pvad (per int pmt)  (* pmt (* (/ (- 1 (/ 1 (expt (+ 1 int) per))) int) (+ 1 int))))

	   ;; These are fv annuity equations
	   (fva  (per int pmt) (* pmt (/ (- (expt (1+ int) per) 1) int)))
	   (naf  (int pmt fut) (/ (log (1+ (/ (* fut int) pmt))) (log (1+ int))))
	   (iap  (per pmt fut) (let ((i 0.001))
				 (labels ((recur ()
						    (cond
						      ((< (- fut (fva per i pmt)) 0) (rund i 5))
						      ((> fut (pva per i pmt)) (progn (setf i (+ i .001)) (recur)))
						      (t nil))))
					   (recur))))
	   (pmtf (per int fut) (/ (* fut int)  (- (expt (1+ int) per) 1)))

	   ;; These are fv annuity due equations
	   (fvad (per int pmt) (* pmt (- (expt (+ 1 int) per) 1) (/ (+ 1 int) int)))
	   
	   ;;the previous but with lump sum at end
	   (pval (per int pmt fut) (+ (* pmt (/ (- 1 (expt (+ 1 int) (- per))) int))
				      (/ fut (expt (+ 1 int) per))))
	   )
	  
  (cond
      ;;lump sum calls pv
      ((and n i pmt fv (eql target 'pv) end) (pval n i pmt fv))

      ;; standard tvm calls
      ((and n i  fv (eql target 'pv)) (pv n i  fv))
      ((and n i  pv (eql target 'fv)) (fv n i  pv))
      ((and n pv fv (eql target 'n))  (i  n pv fv))
      ((and i pv fv (eql target 'i))  (n  i pv fv))
  
      ;; pva calls
      ((and end n i  pmt (eql target 'pv))  (pva  n i  pmt))
      ((and end i pv pmt (eql target 'n))   (nap  i pv pmt))
      ((and end n pv pmt (eql target 'i))   (iap  n pv pmt))
      ((and end n i  pv  (eql target 'pmt)) (pmtp n i  pv))

      ;;pvad calls
      ((and (null end) n i pmt (eql target 'pv)) (pvad n i pmt))

      ;;fva calls 
      ((and end n i   pmt (eql target 'fv))  (fva  n i   pmt))
      ((and end i pmt fv  (eql target 'n))   (naf  i pmt fv))
      ((and end n fv  pmt (eql target 'i))   nil) ;;iap - figure out how to find interest
      ((and end n i   fv  (eql target 'pmt)) (pmtf n i   fv))

      ;;fvad calls
      ((and (null end) n i pmt (eql target 'fv)) (fvad n i pmt))

      ;; error
      (t (error "Whoops!")))))
  
					
					;;EOF

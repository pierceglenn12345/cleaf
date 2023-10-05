;;;; time-value.lisp

(in-package #:trading-core)

(declaim (optimize (speed 0) (safety 3)))


(defun intr (infl rtrn)
  (- (/ (1+ rtrn)
        (1+ infl))
     1))

(defun pv (fv intr yrs &optional (n 1))
  "present value"
  (/ fv (expt
         (+ 1 (/ intr n))
         (* n yrs))))

(defun pvcont (fl r tm)
  (/ fl
     (expt e (* r tm))))

(defun pva (pmt intr yrs)
  "present value of an annuity"
  (* pmt (/ (- 1 (expt (+ 1 intr)
                       (* -1 yrs)))
            intr)))

(defun pvad (pmt intr yrs)
  "present value of annuity due"
  (* pmt (* (/ (- 1
                  (/ 1
                     (expt (+ 1 intr) yrs)))
               intr)
            (+ 1 intr))))

(defun fv (pv intr yrs &optional (n 1))
  "future value"
  (* pv (expt
         (+ 1 (/ intr n))
         (* n yrs))))

(defun fvcont (pv r tm)
  (* pv (expt e (* r tm))))

(defun fva (pmt intr yrs)
  "future value of an annuity"
  (* pmt (/ (- (expt (1+ intr) yrs) 1)
            intr)))

(defun fvad (pmt intr yrs)
  "future value of annuity due"
  (* pmt
     (- (expt (+ 1 intr) yrs)
        1)
     (/ (+ 1 intr)
        intr)))

(defun perp (int div)
  (/ div int))

(defun perpy (pv pmt)
  (/ pmt pv))

(defun perpmt (pv int)
  (* pv int))

(defun perpgrθ (div int grθ)
  (/ div
     (- int grθ)))

(defun pvgrθ (pmt int grθ yrs)
  (* (/ pmt
        (- int grθ))
     (1- (expt (/ (1+ grθ)
                  (1+ int))
               yrs))))

(defun ppmtgrθ (pv i grθ n)
  (* pv (/ (- i grθ)
           (1- (expt (/ (1+ grθ)
                        (1+ i))
                     n)))))

(defun fvgrθ (pmt i grθ n)
  (* pmt (/ (- (expt (1+ i) n) (expt (1+ grθ) n))
            (- i grθ))))

(defun fpmtgrθ (fv i grθ n)
  (* fv (/ (- i grθ)
           (- (expt (1+ i) n) (expt (1+ grθ) n)))))

(defun dblt (r)
  (/ (log 2)
     (log (1+ r))))

(defun dblcont (r)
  (/ (log 2)
     R))

(defun r72 (r)
  (/ 72 r))
 
(defun npv (cashflow rate)
  (if (null cashflow)
      0
      (+ (car cashflow)
         (* (dfactor rate)
            (npv (cdr cashflow) rate)))))

;;EOF

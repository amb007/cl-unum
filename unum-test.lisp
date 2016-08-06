;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2016 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :unum-user)

(deftest make-env.test-1
  (let ((env (make-env 4 5)))
    (and (eq (ess env) 4) (eq (fss env) 5)))
  t)

(deftest make-env.test-2
  (catch-error (make-env 4.5 5))
  caught)

(deftest make-env.test-3
  (catch-error (make-env -4 5))
  caught)

(deftest make-env.test-4
  (catch-error (make-env 4 5.5))
  caught)

(deftest make-env.test-5
  (catch-error (make-env 4 -5))
  caught)

(deftest make-unum.test-1
  (catch-error (make-unum :sign t))
  caught)

(deftest make-unum.test-2
  (catch-error (make-unum :expo 1.0))
  caught)

(deftest make-unum.test-3
  (catch-error (make-unum :frac 1.0))
  caught)

(deftest make-unum.test-4
  (catch-error (make-unum :ubit t))
  caught)

(deftest make-unum.test-5
  (catch-error (make-unum :ubit -1))
  caught)

(deftest make-unum.test-6
  (catch-error (make-unum :e-size 1.0))
  caught)

(deftest make-unum.test-7
  (catch-error (make-unum :e-size 0))
  caught)

(deftest make-unum.test-8
  (catch-error (make-unum :f-size 1.0))
  caught)

(deftest make-unum.test-9
  (catch-error (make-unum :f-size 0))
  caught)

(deftest make-unum.test-10
  (catch-error (make-unum :ess 1.0))
  caught)

(deftest make-unum.test-11
  (catch-error (make-unum :ess -1))
  caught)

(deftest make-unum.test-12
  (catch-error (make-unum :fss 1.0))
  caught)

(deftest make-unum.test-13
  (catch-error (make-unum :fss -1))
  caught)

(deftest make-unum.test-14
  (catch-error (make-unum :expo 256))
  caught)

(deftest make-unum.test-14b
  (eqlu (make-unum :e-size 8) (make-unum :e-size 8))
  t)

(deftest make-unum.test-14c
  (catch-error (make-unum :e-size 9))
  caught)

(deftest make-unum.test-14d
  (eqlu (make-unum :expo 255 :e-size 8) (make-unum :expo 255 :e-size 8))
  t)

(deftest make-unum.test-15
  (catch-error (make-unum :expo 255 :e-size 17 :frac 255 :f-size 33))
  caught)

(deftest make-unum.test-16
  (catch-error (make-unum :expo 16 :e-size 4 :frac 255 :f-size 8))
  caught)

(deftest make-unum.test-17
  (catch-error (make-unum :expo 15 :e-size 4 :frac 256 :f-size 8))
  caught)

(deftest make-unum.test-18
  (eqlu (make-unum :expo 255) (make-unum :expo 255))
  t)

(deftest make-unum.test-19
  (catch-error (make-unum :f-size 17))
  caught)

(deftest unum.test-1
  (let ((*env* (make-env 2 2)))
    (eqlu (unum -126)
	  (make-unum :sign '- :expo 13 :frac -1 :ubit 1 :e-size 4 :f-size 4)))
  t)

(deftest unum.test-2
  (let ((*env* (make-env 1 1)))
    (eqlu (unum -3/4)
	  (make-unum :sign '- :frac 3 :e-size 2 :f-size 2)))
  t)

(deftest inc-e-size.test-1
  (let ((u (make-unum)))
    (eqlu (inc-e-size u) (copy-unum u :e-size (1+ (e-size u)))))
  t)

(deftest inc-e-size.test-1b
  (let ((u (make-unum :sign '-)))
    (eqlu (inc-e-size u) (make-unum :e-size 2)))
  t)

(deftest inc-e-size.test-2
  (let* ((u (make-unum :expo 1))
	 (r (to-real u))
	 (u2 (inc-e-size u))
	 (r2 (to-real u2)))
    (and
     (eql r r2)
     (eqlu u2 (make-unum :expo 2 :e-size 2))))
  t)

(deftest inc-e-size.test-3
  (let* ((u (make-unum :expo 1 :e-size 4))
	 (r (to-real u))
	 (u2 (inc-e-size u))
	 (r2 (to-real u2)))
    (and
     (eql r r2)
     (eqlu u2 (make-unum :expo 9 :e-size 5))))
  t)

(deftest inc-e-size.test-4
  (let ((u (make-unum :e-size 8)))
    (eq (inc-e-size u) u))
  t)

(deftest inc-e-size.test-5
  (let* ((u (make-unum :expo 1 :frac 1 :e-size 4 :f-size 16))
	 (r (to-real u))
	 (u2 (inc-e-size u))
	 (r2 (to-real u2)))
    (and
     (eql r r2)
     (eqlu u2 (make-unum :expo 9 :frac 1 :e-size 5 :f-size 16))))
  t)

(deftest inc-e-size.test-6
  (let* ((u (make-unum :frac 1 :e-size 2 :f-size 8))
	 (r (to-real u))
	 (u2 (inc-e-size u))
	 (r2 (to-real u2)))
    (and
     (eql r r2)
     (eqlu u2 (make-unum :frac 4 :e-size 3 :f-size 8))))
  t)

(deftest inc-e-size.test-6b
  (let* ((u (make-unum :frac 15 :e-size 3 :f-size 8))
	 (r (to-real u))
	 (u2 (inc-e-size u))
	 (r2 (to-real u2)))
    (and
     (eql r r2)
     (eqlu u2 (make-unum :frac (ash 15 4) :e-size 4 :f-size 8))))
  t)

(deftest inc-e-size.test-7
  (let* ((u (make-unum :frac 1 :e-size 6 :f-size 8))
	 (r (to-real u))
	 (u2 (inc-e-size u))
	 (r2 (to-real u2)))
    (and
     (eql r r2)
     (eqlu u2 (make-unum :expo 25 :e-size 7 :f-size 8))))
  t)

(deftest inc-e-size.test-7b
  (let* ((u (make-unum :frac 255 :e-size 6 :f-size 8))
	 (r (to-real u))
	 (u2 (inc-e-size u))
	 (r2 (to-real u2)))
    (and
     (eql r r2)
     (eqlu u2 (make-unum :expo 32 :frac 254 :e-size 7 :f-size 8))))
  t)

(deftest dec-e-size.test-1
  (let ((u (make-unum :e-size 1)))
    (eq (dec-e-size u) u))
  t)

(deftest dec-e-size.test-2
  (let ((u (unum-inf *env*)))
    (eq (dec-e-size u) u))
  t)

(deftest dec-e-size.test-3
  (let ((u (unum-nan *env*)))
    (eq (dec-e-size u) u))
  t)

(deftest dec-e-size.test-4
  (let* ((u (make-unum :frac -1 :e-size 3 :f-size 4))
	 (r (to-real u))
	 (u2 (dec-e-size u))
	 (r2 (to-real u2)))
    (and
     (<= (car r2) r (cdr r2))
     (eqlu u2 (make-unum :frac 3 :ubit 1 :e-size 2 :f-size 4))))
  t)

(deftest dec-e-size.test-4b
  (let* ((u (unum 0.234375))
	 (r (to-real u))
	 (u2 (dec-e-size u))
	 (r2 (to-real u2)))
    (and
     (<= (car r2) r (cdr r2))
     (eqlu u2 (make-unum :frac 7 :ubit 1 :e-size 3 :f-size 3))))
  t)

(deftest dec-e-size.test-5
  (let* ((u (make-unum :expo 1 :frac -1 :e-size 3 :f-size 4))
	 (r (to-real u))
	 (u2 (dec-e-size u))
	 (r2 (to-real u2)))
    (and
     (<= (car r2) r (cdr r2))
     (eqlu u2 (make-unum :frac 7 :ubit 1 :e-size 2 :f-size 4))))
  t)

(deftest dec-e-size.test-5b
  (let* ((u (make-unum :expo 2 :frac -1 :e-size 4 :f-size 4))
	 (r (to-real u))
	 (u2 (dec-e-size u))
	 (r2 (to-real u2)))
    (and
     (<= (car r2) r (cdr r2))
     (eqlu u2 (make-unum :frac 3 :ubit 1 :e-size 3 :f-size 4))))
  t)

(deftest dec-e-size.test-6
  (let* ((u (make-unum :expo 4 :frac -1 :e-size 4 :f-size 4))
	 (r (to-real u))
	 (u2 (dec-e-size u))
	 (r2 (to-real u2)))
    (and
     (<= (car r2) r (cdr r2))
     (eqlu u2 (make-unum :frac -1 :ubit 1 :e-size 3 :f-size 4))))
  t)

(deftest dec-e-size.test-6b
  (let* ((u (make-unum :expo 5 :frac -1 :e-size 4 :f-size 4))
	 (r (to-real u))
	 (u2 (dec-e-size u))
	 (r2 (to-real u2)))
    (and
     (eql r r2)
     (eqlu u2 (make-unum :expo 1 :frac -1 :e-size 3 :f-size 4))))
  t)

(deftest dec-e-size.test-6c
  (let* ((u (make-unum :expo 3 :frac -1 :e-size 4 :f-size 4))
	 (r (to-real u))
	 (u2 (dec-e-size u))
	 (r2 (to-real u2)))
    (and
     (<= (car r2) r (cdr r2))
     (eqlu u2 (make-unum :frac 7 :ubit 1 :e-size 3 :f-size 4))))
  t)

(deftest dec-e-size.test-7
  (let* ((u (make-unum :expo 8 :frac -1 :e-size 4 :f-size 4))
	 (r (to-real u))
	 (u2 (dec-e-size u))
	 (r2 (to-real u2)))
    (and
     (eql r r2)
     (eqlu u2 (make-unum :expo 4 :frac -1 :e-size 3 :f-size 4))))
  t)

(deftest dec-e-size.test-8
  (let* ((u (make-unum :expo 13 :frac -1 :e-size 4 :f-size 4))
	 (r (to-real u))
	 (u2 (dec-e-size u))
	 (r2 (to-real u2)))
    (and
     (<= (car r2) r)
     (eql (cdr r2) (lisp-inf (cdr r2)))
     (eqlu u2 (make-unum :expo -1 :frac -1 :ubit 1 :e-size 3 :f-size 4))))
  t)

(deftest inc-f-size.test-1
  (let ((u (make-unum :f-size 16)))
    (eq (inc-f-size u) u))
  t)

(deftest inc-f-size.test-2
  (let ((u (make-unum :frac 3 :f-size 4)))
    (eqlu (inc-f-size u) (make-unum :frac 6 :f-size 5)))
  t)

(deftest dec-f-size.test-1
  (let ((u (make-unum :e-size 1)))
    (eq (dec-f-size u) u))
  t)

(deftest dec-f-size.test-2
  (let ((u (unum-inf *env*)))
    (eq (dec-f-size u) u))
  t)

(deftest dec-f-size.test-3
  (let ((u (unum-nan *env*)))
    (eq (dec-f-size u) u))
  t)

(deftest dec-f-size.test-4
  (let* ((u (make-unum :frac -1 :e-size 3 :f-size 4))
	 (r (to-real u))
	 (u2 (dec-f-size u))
	 (r2 (to-real u2)))
    (and
     (<= (car r2) r (cdr r2))
     (eqlu u2 (make-unum :frac -1 :ubit 1 :e-size 3 :f-size 3))))
  t)

(deftest dec-f-size.test-4b
  (let* ((u (make-unum :frac -2 :e-size 3 :f-size 4))
	 (r (to-real u))
	 (u2 (dec-f-size u))
	 (r2 (to-real u2)))
    (and
     (eql r r2)
     (eqlu u2 (make-unum :frac -1 :e-size 3 :f-size 3))))
  t)

(deftest dec-f-size.test-5
  (let* ((u (make-unum :frac 1 :e-size 3 :f-size 1))
	 (r (to-real u))
	 (u2 (dec-f-size u))
	 (r2 (to-real u2)))
    (and (eql r r2) (eq u u2)))
  t)

(deftest unify+.test-1
  (let ((ub (make-ubound (unum 1) (unum 2))))
    (eq (unify+ ub) ub))
  t)

(deftest unify+.test-1b
  (let ((ub (ubound 1)))
    (eqlu (unify+ ub) ub))
  t)

(deftest unify+.test-2
  (let ((ub (make-ubound (unum 1) (unum 1))))
    (eqlu (unify+ ub) ub))
  t)

(deftest unify+.test-3
  (let ((ub (make-ubound (unum 0) (unum 1))))
    (eq (unify+ ub) ub))
  t)

(deftest unify+.test-3b
  (let ((ub (make-ubound (unum 1) (unum 2))))
    (eq (unify+ ub) ub))
  t)

(deftest unify+.test-3c
  (let ((ub (make-ubound (unum 2) (unum 3))))
    (eq (unify+ ub) ub))
  t)

(deftest unify+.test-3d
  (let ((ub (make-ubound (unum 3) (unum 4))))
    (eq (unify+ ub) ub))
  t)

(deftest unify+.test-4
  (let ((u1 (unum 2.5))
	(u2 (unum 2.5 :env (make-env 4 5))))
    (eqlu (make-ubound u1 u1) (unify+ (make-ubound u2 u1))))
  t)

(deftest unify+.test-5
  (eqlu (unify+ (make-ubound (unum 1.2) (unum 1.3)))
	  (ubound (make-unum :frac 2 :ubit 1 :f-size 2)))
  t)

(deftest unify+.test-6
  (let* ((u (unum 1.2))
	 (ub (ubound u)))
    (eqlu (gbound (unify+ ub)) (gbound ub)))
  t)

(deftest unify.test-1
  (eqlu
   (unify (make-ubound (unum 23.1) (unum 23.9)))
   (ubound (make-unum :expo 7 :frac 7 :ubit 1 :e-size 3 :f-size 4)))
  t)

(deftest compress.test-1
  (eqlu
   (compress (make-ubound (unum 23.1) (unum 23.9)) 2.5)
   (ubound (make-unum :expo 7 :frac 7 :ubit 1 :e-size 3 :f-size 4)))
  t)

(deftest compress.test-2
  (let ((ub (make-ubound (unum 23.1) (unum 23.9))))
    (eqlu (compress ub 3) ub))
  t)

(deftest greaterp.test-1
  (greaterp (ubound (copy-unum (unum 1) :ubit 1)) (ubound (unum 1)))
  t)

(deftest greaterp.test-2
  (greaterp
   (make-ubound
    (unum 0) (make-unum :expo 14 :frac 1 :ubit 1 :e-size 6 :f-size 1))
   (ubound 0))
  nil)

(deftest lessp.test-1
  (lessp (ubound (unum 1)) (ubound (copy-unum (unum 1) :ubit 1)))
  t)

(deftest lessp.test-2
  (lessp
   (make-ubound
    (unum 0) (make-unum :expo 14 :frac 1 :ubit 1 :e-size 6 :f-size 1))
   (ubound 0))
  nil)

(deftest expo-small.test-1
  (expo-small
   (make-ubound
    (copy-unum (max-unum *env* :sign '-) :ubit 1)
    (max-unum *env* :sign '-)))
  t)

(deftest expo-small.test-2
  (expo-small
   (make-ubound
    (max-unum *env*)
    (copy-unum (max-unum *env*) :ubit 1)))
  t)

(deftest expo-small.test-3
  (expo-small
   (plus (ubound (max-unum *env*)) (ubound 1)))
  t)

(deftest frac-small.test-1
  (frac-small (make-ubound (unum 10) (unum 11)))
  t)

(deftest frac-small.test-2
  (let ((*env* (make-env 3 3)))
    (frac-small (ubound (copy-unum (unum 500) :ubit 1))))
  nil)

(deftest env-small.test-1
  (let ((*env* (make-env 2 2)))
    (declare (special *env*))
    (and
     (eqlu
      (block
	  done
	(tagbody
	 again
	   (loop
	      with one = (ubound 1)
	      for i below 1000
	      and sum = (ubound 0) then (plus sum one)
	      when (expo-small sum)
	      do (progn
		   (setf *env* (make-env (1+ (ess *env*)) (fss *env*)))
		   (go again))
	      when (frac-small sum)
	      do (progn
		   (setf *env* (make-env (ess *env*) (1+ (fss *env*))))
		   (go again))
	      finally (return-from done sum))))
      (let ((*env* (make-env 3 4))) (ubound 1000)))
     (eq (ess *env*) 3)
     (eq (fss *env*) 4)))
  t)

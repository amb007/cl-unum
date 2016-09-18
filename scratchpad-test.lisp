;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2016 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :unum-user)

(deftest negate-gnum.test-1
  (let ((g (make-gnum :f-sign '-))
	(z (make-gnum)))
    (eqlu (negate g) z))
  t)

(deftest negate-gbound.test-1
  (let* ((g1 (make-gnum :f-sign '-))
	 (g2 (make-gnum))
	 (gb (make-gbound g1 g2)))
    (eqlu (negate gb) gb))
  t)

(deftest negate-gbound.test-2
  (let* ((g1 (make-gnum :a-frac 3 :f-sign '-))
	 (g2 (make-gnum :a-frac 5))
	 (gb (make-gbound g1 g2)))
    (eqlu (negate gb) (make-gbound (negate g2) (negate g1))))
  t)

(deftest plus.test-1
  (let ((ub1 (make-ubound (unum -1.25) (unum 1.5)))
	(ub2 (make-ubound (unum 3.25) (unum 6.25))))
    (eqlu (plus ub1 ub2)
	    (make-ubound (unum (+ -1.25 3.25)) (unum (+ 1.5 6.25)))))
  t)

(deftest times.test-1
  (let ((ub1 (make-ubound (unum -1.25) (unum 1.5)))
	(ub2 (make-ubound (unum 3.25) (unum 6.25))))
    (eqlu (times ub1 ub2)
	    (make-ubound (unum (* -1.25 6.25)) (unum (* 1.5 6.25)))))
  t)

(deftest divide.test-1
  (let ((ub1 (make-ubound (unum 1.25) (unum 1.75)))
	(ub2 (make-ubound (unum 0.25) (unum 0.5))))
    (eqlu (divide ub1 ub2)
	    (make-ubound (unum (/ 1.25 0.5)) (unum (/ 1.75 0.25)))))
  t)

(deftest fma.test-1
  (let ((ub1 (make-ubound (unum 1.25) (unum 1.75)))
	(ub2 (make-ubound (unum 0.25) (unum 0.5)))
	(ub3 (make-ubound (unum 0.25) (unum 0.5))))
    (eqlu (fma ub1 ub2 ub3)
	    (make-ubound (unum 0.5625) (unum 1.375))))
  t)

(deftest fam.test-1
  (eqlu (fam 1 2 3) (ubound 9))
  t)

(deftest fdot.test-1
  (let ((v1 (vector 1 2))
	(v2 (vector 3 4)))
    (eqlu (fdot v1 v2) (ubound 11)))
  t)

(deftest fsum.test-1
  (let ((v (vector 1 2 3 4)))
    (eqlu (fsum v) (ubound 10)))
  t)

(deftest fprod.test-1
  (let ((v (vector 1 2 3 4)))
    (eqlu (fprod v) (ubound 24)))
  t)

(deftest fprod-ratio.test-1
  (let ((v1 (vector 1 2 3))
	(v2 (vector 1.5 2)))
    (eqlu (fprod-ratio v1 v2) (ubound 2)))
  t)

(deftest square.test-1
  (eqlu (square (make-gbound (gnum -3) (gnum 2 :open-p t)))
	  (make-gbound (make-gnum) (gnum 9)))
  t)

(deftest square.test-2
  (eqlu (square (make-gbound (gnum -3 :open-p t) (gnum 2)))
	  (make-gbound (make-gnum) (gnum 9 :open-p t)))
  t)

(deftest square.test-3
  (eqlu (square 3.25) (ubound 10.5625))
  t)

(deftest sqroot.test-1
  (eqlu (sqroot (make-gbound (gnum 4) (gnum 9 :open-p t)))
	  (make-gbound (gnum 2) (gnum 3 :open-p t)))
  t)

(deftest sqroot.test-2
  (nan-p (sqroot (make-gbound (gnum -4) (gnum 9))))
  t)

(deftest power.test-1
  (eqlu (power (make-gbound (gnum 2) (gnum 3))
		 (make-gbound (gnum 2 :open-p t) (gnum 3)))
	  (make-gbound (gnum 4 :open-p t) (gnum 27)))
  t)

(deftest power.test-2
  (eqlu (power (make-gbound (gnum 2) (gnum 3.5))
		 (make-gbound (gnum -2 :open-p t) (gnum 3)))
	  (make-gbound (gnum 4/49 :open-p t) (gnum 42.875)))
  t)

(deftest power.test-3
  (eqlu (power (make-gbound (gnum 2) (gnum 3.5))
		 (make-gbound (gnum -2) (gnum 3)))
	  (make-gbound (gnum 4/49) (gnum 42.875)))
  t)

(deftest power.test-4
  (eqlu (power -4 -2) (ubound 1/16))
  t)

(deftest e-power.test-1
  (eqlu (e-power 2)
	  (ubound (make-unum :expo 3 :frac 55526 :ubit 1 :e-size 2 :f-size 16)))
  t)

(deftest intersect.test-1
  (let ((gb1 (gbound (make-ubound (unum 1.2) (unum 1.6))))
	(gb2 (gbound (make-ubound (unum 1.3) (unum 1.9))))
	(gbi (gbound (make-ubound (unum 1.3) (unum 1.6)))))
    (eqlu (intersect gb1 gb2) gbi))
  t)

(deftest intersect.test-2
  (let ((gb (gbound (make-ubound (unum 1.3) (unum 1.9)))))
    (nan-p (intersect (gbound-nan) gb)))
  t)

(deftest intersect.test-2b
  (let ((gb (gbound (make-ubound (unum 1.3) (unum 1.9)))))
    (nan-p (intersect gb (gbound-nan))))
  t)

(deftest intersect.test-3
  (let ((gb1 (gbound (make-ubound (unum 1.2) (unum 1.3))))
	(gb2 (gbound (make-ubound (unum 1.3) (unum 1.9)))))
    (eqlu (intersect gb1 gb2) (gbound (unum 1.3))))
  t)

(deftest intersect.test-4
  (let ((gb1 (gbound (make-ubound (unum 1.2) (unum 1.3))))
	(gb2 (gbound (make-ubound (unum 1.2) (unum 1.9)))))
    (eqlu (intersect gb1 gb2) gb1))
  t)

(deftest intersect.test-5
  (let ((gb1 (gbound (make-ubound (unum 1.3) (unum 1.9))))
	(gb2 (gbound (make-ubound (unum 1.2) (unum 1.4))))
	(gbi (gbound (make-ubound (unum 1.3) (unum 1.4)))))
    (eqlu (intersect gb1 gb2) gbi))
  t)

(deftest intersect.test-6
  (let ((gb1 (gbound (make-ubound (unum 1.3) (unum 1.5))))
	(gb2 (gbound (make-ubound (unum 1.6) (unum 1.7)))))
    (nan-p (intersect gb1 gb2)))
  t)

(deftest intersect.test-6b
  (let ((gb1 (gbound (make-ubound (unum 1.3) (unum 1.5))))
	(gb2 (gbound (make-ubound (unum 1.5) (unum 1.7))))
	(gbi (gbound (make-ubound (unum 1.5)))))
    (eqlu (intersect gb1 gb2) gbi))
  t)

(deftest intersect.test-7
  (let ((gb1 (gbound (make-ubound (unum 1.3) (unum 1.6))))
	(gb2 (gbound (make-ubound (unum 1.5) (unum 1.7))))
	(gbi (gbound (make-ubound (unum 1.5) (unum 1.6)))))
    (eqlu (intersect gb1 gb2) gbi))
  t)

(deftest intersect.test-8
  (let ((gb1 (gbound (make-ubound (unum 1.3) (unum 1.6))))
	(gb2 (gbound (make-ubound (unum 1.6) (unum 1.7))))
	(gbi (gbound (make-ubound (unum 1.6)))))
    (eqlu (intersect gb1 gb2) gbi))
  t)

(deftest intersect.test-9
  (let ((gb1 (gbound (make-ubound (unum 1.2) (unum 1.3))))
	(gb2 (gbound (make-ubound (unum 1.4) (unum 1.5)))))
    (nan-p (intersect gb1 gb2)))
  t)

(deftest intersect.test-10
  (let ((gb1 (gbound (make-ubound (unum 1.2) (copy-unum (unum 1.25) :ubit 1))))
	(gb2 (gbound (make-ubound (unum 1.5) (unum 1.6)))))
    (nan-p (intersect gb1 gb2)))
  t)

(deftest intersect.test-11
  (let ((gb1 (gbound (make-ubound (unum 1.2) (copy-unum (unum 1.25) :ubit 1))))
	(gb2 (gbound (make-ubound (unum 1.22) (unum 1.25))))
	(gbi (gbound (make-ubound (unum 1.22) (unum 1.25)))))
    (eqlu (intersect gb1 gb2) gbi))
  t)

(deftest intersect.test-11b
  (let ((gb1 (gbound (make-ubound (unum 1.2) (unum 1.25))))
	(gb2 (gbound (make-ubound (unum 1.22) (unum 1.25)))))
    (eqlu (intersect gb1 gb2) gb2))
  t)

(deftest intersect.test-11c
  (let ((gb1 (gbound (make-ubound (unum 1.2) (unum 1.26))))
	(gb2 (gbound (make-ubound (unum 1.22) (unum 1.26)))))
    (eqlu (intersect gb1 gb2) gb2))
  t)

(deftest intersect.test-12
  (let ((gb1 (gbound (make-ubound (unum 1.2) (unum 1.25))))
	(gb2 (gbound (make-ubound (unum 1.1) (unum 1.3)))))
    (eqlu (intersect gb1 gb2) gb1))
  t)

(deftest intersect.test-12b
  (let ((gb1 (gbound (make-ubound (unum 1.2) (unum 1.26))))
	(gb2 (gbound (make-ubound (unum 1.1) (unum 1.26)))))
    (eqlu (intersect gb1 gb2) gb1))
  t)

(deftest intersect.test-12c
  (let ((gb1 (gbound (make-ubound (unum 1.2) (unum 1.3))))
	(gb2 (gbound (make-ubound (unum 1.1) (unum 1.26))))
	(gbi (gbound (make-ubound (unum 1.2) (unum 1.26)))))
    (eqlu (intersect gb1 gb2) gbi))
  t)

(deftest intersect.test-12d
  (let ((gb1 (gbound (make-ubound (unum 1.2) (unum 1.25))))
	(gb2 (gbound (make-ubound (unum 1.1) (copy-unum (unum 1.25) :ubit 1)))))
    (eqlu (intersect gb1 gb2) gb1))
  t)

(deftest intersect.test-12e
  (let ((gb1 (gbound (make-ubound (unum 1.2) (unum 1.25))))
	(gb2 (gbound (make-ubound (unum 1.1) (unum 1.25)))))
    (eqlu (intersect gb1 gb2) gb1))
  t)

(deftest intersect.test-13
  (let ((gb1 (gbound (make-ubound (unum 1.2) (unum 1.25))))
	(gb2 (gbound (make-ubound (unum 1) (unum 1.1)))))
    (nan-p (intersect gb1 gb2)))
  t)

(deftest intersect.test-14
  (let ((gb1 (gbound (make-ubound (unum 1.25) (unum 1.25))))
	(gb2 (gbound (make-ubound (unum 1.2) (unum 1.25))))
	(gbi (gbound (unum 1.25))))
    (eqlu (intersect gb1 gb2) gbi))
  t)

(deftest intersect.test-14b
  (let ((gb1 (gbound (make-ubound (unum 1.2) (unum 1.2))))
	(gb2 (gbound (make-ubound (unum 1) (copy-unum (unum 1.2) :ubit 0)))))
    (nan-p (intersect gb1 gb2)))
  t)

(deftest ch14.test-1
  (let ((*env* (make-env 3 6)))
    (with-scratchpad ()
      (and
       (lessp
	(ubound 6.13)
	(loop for i below 12
	   for u0 = (ubound 2) then u1
	   for u1 = (ubound -4) then u2
	   for u2 = (plus
		     (minus (ubound 111) (divide (ubound 1130) u1))
		     (divide (ubound 3000) (times u1 u0)))
	   finally (return u2))
	(ubound 6.1505))
       (<= 20148 *ubits-moved* 20361)
       (<= 300 *unums-moved* 302)
       (eql *ubounds-moved* 180))))
  t)

(deftest ch14.test-2
  (labels ((%e (z) (if (jointp z (ubound 0))
		       (ubound 1)
		       (divide (minus (e-power z) (ubound 1)) z)))
	   (%q (x) (let ((v (sqroot (plus (square x) (ubound 1)))))
		     (minus (absolute (minus x v))
			    (divide (ubound 1) (plus x v)))))
	   (%h (x) (%e (square (%q x)))))
    (and
     (let ((o (ubound 1)))
       (with-scratchpad ()
	 (every #'eqlu (list o o o o)
		(list (%h (ubound 15)) (%h (ubound 16))
		      (%h (ubound 17)) (%h (ubound 9999))))))
     (let* ((*env* (make-env 0 0))
	    (o (ubound 1)))
       (with-scratchpad ()
	 (every #'eqlu (list o o o o)
		(list (%h (ubound 15)) (%h (ubound 16))
		      (%h (ubound 17)) (%h (ubound 9999))))))))
  t)

(defun rump (x y)
  (let* ((x (ubound x))
	 (y (ubound y))
	 (x2 (square x))
	 (y2 (square y))
	 (y4 (power y (ubound 4)))
	 (y6 (power y (ubound 6)))
	 (y8 (power y (ubound 8))))
    (plus (times (ubound 333.75) y6)
	  (times
	   x2 (minus (times (ubound 11) x2 y2)
		     y6 (times (ubound 121) y4) (ubound 2)))
	  (times (ubound 5.5) y8)
	  (divide x (times (ubound 2) y)))))

(deftest ch14.test-3
  (with-scratchpad ()
    (let* ((*env* (make-env 3 7))
	   (r (rump 77617 33096)))
      (<= -0.8274 (to-rational (lo r))
	  (to-rational (hi* r)) -0.8273)))
  t)

(deftest ch14.test-4
  (with-scratchpad ()
    (let ((r (calculate (rump 77617 33096))))
      (and (eql (ess (lo r)) 3) (eql (fss (lo r)) 7))))
  t)

(deftest ch14.test-5
  (let ((r1
	 (with-scratchpad ()
	   (let ((*env* (make-env 3 5))
		 (a (ubound 3))
		 (b (ubound 100))
		 (c (ubound 2)))
	     (divide
	      (minus (sqroot (minus (square b) (times (ubound 4) a c))) b)
	      (times (ubound 2) a))))))
    (lessp (ubound -0.0200122) r1 (ubound -0.0200119)))
  t)

(deftest ch14.test-6
  (with-scratchpad ()
    (let* ((*env* (make-env 3 5))
	   (a (ubound 25510582))
	   (b (ubound 52746197))
	   (c (ubound 80143857))
	   (d (ubound 165707065))
	   (u (ubound 79981812))
	   (v (ubound 251270273))
	   (det (fdot (make-ubarray (vector a c))
		      (make-ubarray (vector d (negate b)))))
	   (x (fdot (make-ubarray (vector u v))
		    (make-ubarray (vector d (negate b)))))
	   (y (fdot (make-ubarray (vector a c))
		    (make-ubarray (vector v (negate u))))))
      (and (eqlu (divide x det) (ubound -1))
	   (eqlu (divide y det) (ubound 2)))))
  t)

(deftest ch17.test-1
  (with-scratchpad ()
    (jointp
     (plus
      (square (make-ubound (unum (lisp-inf -1.0d0))
			   (unum (lisp-inf 1.0d0))))
      (ubound 1))
     (ubound 0)))
  nil)

(deftest ch17.test-2
  (flet ((%quad (x)
	   (poly
	    (make-ubarray (vector (ubound 2) (ubound 100) (ubound 3)))
	    x)))
    (let* ((*env* (make-env 0 0))
	   (dom (make-ubound (open-inf-unum *env* :sign '-)
			     (open-inf-unum *env*))))
      (jointp (%quad dom) (ubound 0))))
  t)

(deftest ch17.test-2b
  (flet ((%quad (x)
	   (poly
	    (make-ubarray (vector (ubound 2) (ubound 100) (ubound 3)))
	    x)))
    (let* ((*env* (make-env 1 1))
	   (dom (make-ubound (open-inf-unum *env* :sign '-)
			     (open-inf-unum *env*))))
      (jointp (%quad dom) (ubound 0))))
  t)

(deftest ch17.test-2c
  (flet ((%quad (x)
	   (poly
	    (make-ubarray (vector (ubound 2) (ubound 100) (ubound 3)))
	    x)))
    (let* ((*env* (make-env 2 2))
	   (dom (make-ubound (open-inf-unum *env* :sign '-)
			     (open-inf-unum *env*))))
      (jointp (%quad dom) (ubound 0))))
  t)

(deftest ch17.test-2d
  (flet ((%quad (x)
	   (poly
	    (make-ubarray (vector (ubound 2) (ubound 100) (ubound 3)))
	    x)))
    (let* ((*env* (make-env 3 2))
	   (dom (make-ubound (open-inf-unum *env* :sign '-)
			     (open-inf-unum *env*))))
      (jointp (%quad dom) (ubound 0))))
  t)

(deftest ch17.test-2e
  (flet ((%quad (x)
	   (poly
	    (make-ubarray (vector (ubound 2) (ubound 100) (ubound 3)))
	    x)))
    (let* ((*env* (make-env 3 3))
	   (dom (make-ubound (open-inf-unum *env* :sign '-)
			     (open-inf-unum *env*))))
      (jointp (%quad dom) (ubound 0))))
  t)

(deftest ch17.test-2f
  (flet ((%quad (x)
	   (poly
	    (make-ubarray (vector (ubound 2) (ubound 100) (ubound 3)))
	    x)))
    (let* ((*env* (make-env 3 4)) ; [4+ 4] or [3 5+] explodes
	   (dom (make-ubound (open-inf-unum *env* :sign '-)
			     (open-inf-unum *env*))))
      (jointp (%quad dom) (ubound 0))))
  t)
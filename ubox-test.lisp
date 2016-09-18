;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2016 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :unum-user)

(deftest favor-e.test-1
  (let* ((u0 (unum 16384))
	 (u1 (favor-e u0)))
    (and (eql (to-rational u0) (to-rational u1)) (> (e-size u1) (e-size u0))))
  t)

(deftest favor-e.test-2
  (let* ((u0 (unum 20000))
	 (u1 (favor-e u0)))
    (and (inexactp u1) (eql (to-rational u1) 19968)
	 (> (e-size u1) (e-size u0))))
  t)

(deftest nbor-hi.test-1
  (let ((*env* (make-env 1 3)))
    (eqlu (nbor-hi (unum 0) -1) (make-unum :e-size 2 :ubit 1)))
  t)

(deftest nbor-hi.test-2
  (let ((*env* (make-env 1 3)))
    (eqlu (nbor-hi (unum 0) -3) (make-unum :e-size 2 :f-size 3 :ubit 1)))
  t)

(deftest nbor-hi.test-3
  (let ((*env* (make-env 1 3)))
    (eqlu (nbor-hi (unum 0) -10) (make-unum :e-size 2 :f-size 8 :ubit 1)))
  t)

(deftest nbor-hi.test-4
  (let* ((*env* (make-env 1 2))
	 (n1 (nbor-hi (max-unum *env*) 0)))
    (and
     (eqlu n1 (make-unum :expo 3 :e-size 2 :frac 14 :f-size 4 :ubit 1))
     (let ((n2 (nbor-hi n1 0)))
       (and
	(inf-p n2)
	(let ((n3 (nbor-hi n2 0)))
	  (and
	   (nan-p n3)
	   (let ((n4 (nbor-hi n3 0)))
	     (nan-p n4))))))))
  t)

(deftest nbor-lo.test-1
  (let ((n1 (nbor-lo (unum 0.625) -1)))
    (and
     (eqlu n1 (make-unum :expo 126 :e-size 8 :f-size 2 :ubit 1))
     (let ((n2 (nbor-lo n1 -1)))
       (and
	(eqlu n2 (make-unum :expo 126 :e-size 8 :f-size 2))
	(let ((n3 (nbor-lo n2 -1)))
	  (eqlu n3 (make-unum :e-size 2 :ubit 1)))))))
  t)

(deftest split.test-1
  (let ((subs (split (make-ubound (unum 1.25) (unum 1.5)))))
    (and
     (eql (length subs) 3)
     (every #'eqlu subs
	    (list (ubound (unum 1.25))
		  (ubound (make-unum :frac 5 :ubit 1 :f-size 3))
		  (ubound (unum 1.5))))))
  t)

(deftest split.test-2
  (let* ((*env* (make-env 1 2))
	 (subs (split (make-ubound (open-inf-unum *env* :sign '-)
				   (open-inf-unum *env*)))))
    (and
     (eql (length subs) 3)
     (every #'eqlu subs
	    (list (ubound (make-unum :sign '- :expo 3 :frac 14 :ubit 1))
		  (ubound (make-unum :sign '- :expo 3 :frac 14))
		  (make-ubound (make-unum :sign '- :expo 3 :frac 6 :ubit 1)
			       (open-inf-unum *env*))))))
  t)

(deftest bisect.test-1
  (let ((sgbs (bisect (make-gbound (gnum 1.25) (gnum 1.5)))))
    (and
     (eql (length sgbs) 2)
     (every #'eqlu sgbs
	    (list (make-gbound (gnum 1.25) (gnum 1.375 :open-p t))
		  (make-gbound (gnum 1.375 :open-p t) (gnum 1.5))))))
  t)

(deftest leftmost.test-1
  (let ((ub1 (ubound (copy-unum (unum 1) :ubit 1)))
	(ub2 (ubound (unum 1))))
    (eq (leftmost ub1 ub2) ub2))
  t)

(deftest leftmost.test-2
  (let ((ub1 (ubound (unum 1)))
	(ub2 (ubound (copy-unum (unum 1) :ubit 1))))
    (eq (leftmost ub1 ub2) ub1))
  t)

(deftest rightmost.test-1
  (let ((ub1 (ubound (copy-unum (unum 1) :ubit 1)))
	(ub2 (ubound (unum 2))))
    (eq (rightmost ub1 ub2) ub2))
  t)

(deftest rightmost.test-2
  (let ((ub1 (ubound (unum 2)))
	(ub2 (ubound (copy-unum (unum 1) :ubit 1))))
    (eq (rightmost ub1 ub2) ub1))
  t)

(deftest unite.test-1
  (let* ((ub (ubound (unum 1)))
	 (ubs (list (ubound (unum 2)) (ubound (copy-unum (unum 1) :ubit 1))))
	 (r (unite ub ubs)))
    (and
     (eql (length r) 2)
     (every #'eqlu r (list (make-ubound (lo ub) (hi* (cadr ubs))) (car ubs)))))
  t)

(deftest unite.test-2
  (let* ((ub (ubound (unum 1)))
	 (r (unite ub nil)))
    (and (eql (length r) 1) (eq (car r) ub)))
  t)

(deftest insert.test-1
  (let* ((ub (ubound (unum 1)))
	 (r (insert ub nil)))
    (and (eql (length r) 1) (eq (car r) ub)))
  t)

(deftest insert.test-2
  (let* ((ub (ubound (unum 1)))
	 (ubs (list (ubound (unum 0.8))))
	 (r (insert ub ubs)))
     (and (eql (length r) 2) (every #'eqlu r (list (car ubs) ub))))
  t)

(deftest insert.test-3
  (let* ((ub (ubound (unum 1)))
	 (ubs (list (ubound (unum 0.8)) (ubound (unum 1.2))))
	 (r (insert ub ubs)))
    (and (eql (length r) 3) (every #'eqlu r (list (car ubs) ub (cadr ubs)))))
  t)

(deftest insert.test-4
  (let* ((ub (ubound (unum 1)))
	 (ubs (list (ubound (copy-unum (unum 0.5) :ubit 1))))
	 (r (insert ub ubs)))
    (and
     (eql (length r) 1)
     (eqlu (car r) (make-ubound (lo (car ubs)) (hi* ub)))))
  t)

(deftest insert.test-4b
  (let* ((ub (ubound (unum 1)))
	 (ubs (list (ubound (copy-unum (unum 0.5) :ubit 1))
		    (ubound (unum 2))))
	 (r (insert ub ubs)))
    (and
     (eql (length r) 2)
     (eqlu (car r) (make-ubound (lo (car ubs)) (hi* ub)))
     (eq (cadr r) (cadr ubs))))
  t)

(deftest insert.test-5
  (let* ((ub (ubound (unum 1)))
	 (ubs (list (ubound (copy-unum (unum 1) :ubit 1))))
	 (r (insert ub ubs)))
    (and
     (eql (length r) 1)
     (eqlu (car r) (make-ubound (lo ub) (hi* (car ubs))))))
  t)

(deftest insert.test-6
  (let* ((ub (ubound (unum 1)))
	 (ubs (list (ubound (copy-unum (unum 0.5) :ubit 1))
		    (ubound (copy-unum (unum 1) :ubit 1))))
	 (r (insert ub ubs)))
    (and
     (eql (length r) 1)
     (eqlu (car r) (make-ubound (lo (car ubs)) (hi* (cadr ubs))))))
  t)

(deftest insert.test-6b
  (let* ((ub (ubound (unum 1)))
	 (ubs (list (ubound (copy-unum (unum 0.5) :ubit 1))
		    (ubound (copy-unum (unum 1) :ubit 1))
		    (ubound (copy-unum (unum 3)))))
	 (r (insert ub ubs)))
    (and
     (eql (length r) 2)
     (eqlu (car r) (make-ubound (lo (car ubs)) (hi* (cadr ubs))))
     (eqlu (cadr r) (caddr ubs))))
  t)

(deftest insert.test-6c
  (let* ((ub (ubound (unum 1)))
	 (ubs (list (ubound (copy-unum (unum 0)))
		    (ubound (copy-unum (unum 0.5) :ubit 1))
		    (ubound (copy-unum (unum 1) :ubit 1))))
	 (r (insert ub ubs)))
    (and
     (eql (length r) 2)
     (eqlu (car r) (car ubs))
     (eqlu (cadr r) (make-ubound (lo (cadr ubs)) (hi* (caddr ubs))))))
  t)

(deftest ch17.test-s2
  (flet ((%quad (x)
	   (poly
	    (make-ubarray (vector (ubound 2) (ubound 100) (ubound 3)))
	    x)))
    (let* ((*env* (make-env 2 5))
	   (dom (make-ubound (open-inf-unum *env* :sign '-)
			     (open-inf-unum *env*)))
	   (zero (ubound 0))
	   (s (solve-ulp (list dom) (lambda (x) (jointp (%quad x) zero)))))
      (and (eql 2 (length s))
	   (every
	    #'eqlu s
	    (list
	     (make-ubound
	      (make-unum :sign '- :expo 12
			 :frac #b00001010100000011010111010011011
			 :ubit 1 :e-size 4 :f-size 32))
	     (make-ubound
	      (make-unum :sign '- :expo 1
			 :frac #b01000111111000000111100011011101
			 :ubit 1 :e-size 4 :f-size 32)))))))
  t)
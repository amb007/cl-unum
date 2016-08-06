;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2016 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

#+cover-unum (cover:annotate t)

(in-package :unum)

(defmacro with-scratchpad (opts &body body)
  (let ((erp-body (unless (member :quiet opts)
		    '((format t "~&;;; Scratchpad transfer: ~D bits, ~D unums, ~
                                 ~D ubounds (~D b/ub)"
		       *ubits-moved* *unums-moved* *ubounds-moved*
		       (float (/ *ubits-moved* *ubounds-moved*)))))))
    `(let ((*ubits-moved* 0)
	   (*unums-moved* 0)
	   (*ubounds-moved* 0))
       (prog1
	   (progn ,@body)
	 ,@erp-body))))

(defmethod negate ((g gnum))
  (if (nan-p g)
      g
      (copy-gnum g :f-sign (if (eq '+ (f-sign g)) '- '+))))

(defmethod negate ((gb gbound))
  (if (nan-p gb)
      gb
      (make-gbound (negate (hi gb)) (negate (lo gb)))))

(defmethod plus2 ((gb1 gbound) (gb2 gbound))
  (if (or (nan-p gb1) (nan-p gb2))
      (make-instance 'gbound :nan-p t)
      (let*
	  ((g1l (lo gb1))
	   (g1ln (eq (f-sign g1l) '-))
	   (g1li (inf-p g1l))
	   (g1lo (open-p g1l))
	   (g1r (hi gb1))
	   (g1rn (eq (f-sign g1r) '-))
	   (g1ri (inf-p g1r))
	   (g1ro (open-p g1r))
	   (g2l (lo gb2))
	   (g2ln (eq (f-sign g2l) '-))
	   (g2li (inf-p g2l))
	   (g2lo (open-p g2l))
	   (g2r (hi gb2))
	   (g2rn (eq (f-sign g2r) '-))
	   (g2ri (inf-p g2r))
	   (g2ro (open-p g2r))
	   (g+l
	    (cond
	      ((and g1ln g1li (not g1lo))
	       (if (and (not g2ln) g2li (not g2lo))
		   (make-gnum :nan-p t :open-p t)
		   (make-gnum :f-sign '- :inf-p t)))
	      ((and g2ln g2li (not g2lo))
	       (if (and (not g1ln) g1li (not g1lo))
		   (make-gnum :nan-p t :open-p t)
		   (make-gnum :f-sign '- :inf-p t)))
	      ((or (and (not g1ln) g1li (not g1lo))
		   (and (not g2ln) g2li (not g2lo)))
	       (make-gnum :inf-p t))
	      ((and g1ln g1li)
	       (if (and (not g2ln) g2li (not g2lo))
		   (make-gnum :inf-p t)
		   (make-gnum :f-sign '- :inf-p t :open-p t)))
	      ((and g2ln g2li)
	       (if (and (not g1ln) g1li (not g1lo))
		   (make-gnum :inf-p t)
		   (make-gnum :f-sign '- :inf-p t :open-p t)))
	      (t (let ((tmp (gnum (+ (to-rational g1l) (to-rational g2l)))))
		   (if (inf-p tmp)
		       (if (eq (f-sign tmp) '+)
			   (gnum (lisp-biggest 1.0d0))
			   (copy-gnum tmp :open-p t))
		       (copy-gnum tmp :open-p (or g1lo g2lo)))))))
	   (g+r
	    (cond
	      ((and g1rn g1ri (not g1ro))
	       (if (and (not g2rn) g2ri (not g2ro))
		   (make-gnum :nan-p t :open-p t)
		   (make-gnum :f-sign '- :inf-p t)))
	      ((and g2rn g2ri (not g2ro))
	       (if (and (not g1rn) g1ri (not g1ro))
		   (make-gnum :nan-p t :open-p t)
		   (make-gnum :f-sign '- :inf-p t)))
	      ((or (and (not g1rn) g1ri (not g1ro))
		   (and (not g2rn) g2ri (not g2ro)))
	       (make-gnum :inf-p t))
	      ((and (not g1rn) g1ri)
	       (if (and g2rn g2ri (not g2ro))
		   (make-gnum :f-sign '- :inf-p t)
		   (make-gnum :inf-p t :open-p t)))
	      ((and (not g2rn) g2ri)
	       (if (and g1rn g1ri (not g1ro))
		   (make-gnum :f-sign '- :inf-p t)
		   (make-gnum :inf-p t :open-p t)))
	      (t (let ((tmp (gnum (+ (to-rational g1r) (to-rational g2r)))))
		   (if (inf-p tmp)
		       (if (eq (f-sign tmp) '-)
			   (gnum (lisp-biggest -1.0d0))
			   (copy-gnum tmp :open-p t))
		       (copy-gnum tmp :open-p (or g1ro g2ro))))))))
	(make-gbound g+l g+r))))

(defmethod plus2 ((ub1 ubound) (ub2 ubound))
  (ubound (plus2 (gbound ub1) (gbound ub2))))

(defmethod plus ((ub1 ubound) (ub2 ubound) &rest ubs)
  (reduce #'plus2 ubs :initial-value (plus2 ub1 ub2)))

(defmethod plus ((f1 real) (f2 real) &rest fs)
  (apply #'plus (ubound f1) (ubound f2) (mapcar #'ubound fs)))

(defmethod minus2 ((gb1 gbound) (gb2 gbound))
  (plus2 gb1 (negate gb2)))

(defmethod minus2 ((ub1 ubound) (ub2 ubound))
  (ubound (minus2 (gbound ub1) (gbound ub2))))

(defmethod minus ((ub1 ubound) (ub2 ubound) &rest ubs)
  (reduce #'minus2 ubs :initial-value (minus2 ub1 ub2)))

(defmethod minus ((f1 real) (f2 real) &rest fs)
  (apply #'minus (ubound f1) (ubound f2) (mapcar #'ubound fs)))

(defun times+l (g1l g2l)
  (let* ((g1lz (unless (inf-p g1l) (eql 0 (a-frac g1l))))
	 (g1li (inf-p g1l))
	 (g1lo (open-p g1l))
	 (g2lz (unless (inf-p g2l) (eql 0 (a-frac g2l))))
	 (g2li (inf-p g2l))
	 (g2lo (open-p g2l)))
    (cond
      ((and g1lz (not g1lo))
       (if (and g2li (not g2lo))
	   (make-gnum :nan-p t :open-p t)
	   g1l))
      ((and g2lz (not g2lo))
       (if (and g1li (not g1lo))
	   (make-gnum :nan-p t :open-p t)
	   g2l))
      ((and g1lz g1lo)
       (if (and g2li (not g2lo))
	   g2l
	   (make-gnum :open-p t)))
      ((and g2lz g2lo)
       (if (and g1li (not g1lo))
	   g1l
	   (make-gnum :open-p t)))
      ((or (and g1li (not g1lo)) (and g2li (not g2lo)))
       (make-gnum :inf-p t))
      (t (let ((tmp (gnum (* (to-rational g1l) (to-rational g2l)))))
	   (copy-gnum tmp :open-p (or g1lo g2lo)))))))

(defun times+r (g1r g2r)
  (let* ((g1rz (unless (inf-p g1r) (eql 0 (a-frac g1r))))
	 (g1ri (inf-p g1r))
	 (g1ro (open-p g1r))
	 (g2rz (unless (inf-p g2r) (eql 0 (a-frac g2r))))
	 (g2ri (inf-p g2r))
	 (g2ro (open-p g2r)))
    (cond
      ((and g1ri (not g1ro))
       (if (and g2rz (not g2ro))
	   (make-gnum :nan-p t :open-p t)
	   g1r))
      ((and g2ri (not g2ro))
       (if (and g1rz (not g1ro))
	   (make-gnum :nan-p t :open-p t)
	   g2r))
      ((and g1ri g1ro)
       (if (and g2rz (not g2ro))
	   g2r
	   (make-gnum :inf-p t :open-p t)))
      ((and g2ri g2ro)
       (if (and g1rz (not g1ro))
	   g1r
	   (make-gnum :inf-p t :open-p t)))
      ((or (and g1rz (not g1ro) (and g2rz (not g2ro))))
       (make-gnum))
      (t (let ((tmp (gnum (* (to-rational g1r) (to-rational g2r)))))
	   (copy-gnum tmp :open-p (or g1ro g2ro)))))))

(defmethod times2 ((gb1 gbound) (gb2 gbound))
  (if (or (nan-p gb1) (nan-p gb2))
      (make-instance 'gbound :nan-p t)
      (let* (lcan
	     rcan
	     (g1l (lo gb1))
	     (g1lz (unless (inf-p g1l) (eql 0 (a-frac g1l))))
	     (g1ln (eq (f-sign g1l) '-))
	     (g1lo (open-p g1l))
	     (g1r (hi gb1))
	     (g1rz (unless (inf-p g1r) (eql 0 (a-frac g1r))))
	     (g1rn (eq (f-sign g1r) '-))
	     (g1ro (open-p g1r))
	     (g2l (lo gb2))
	     (g2lz (unless (inf-p g2l) (eql 0 (a-frac g2l))))
	     (g2ln (eq (f-sign g2l) '-))
	     (g2lo (open-p g2l))
	     (g2r (hi gb2))
	     (g2rz (unless (inf-p g2r) (eql 0 (a-frac g2r))))
	     (g2rn (eq (f-sign g2r) '-))
	     (g2ro (open-p g2r)))
	(when (and (not g1ln) (not g2ln))
	  (pushnew (times+l g1l g2l) lcan :test #'eqlu))
	(when (and (or (and g1rn (not g1rz)) (and g1rz g1ro))
		   (or (and g2rn (not g2rz)) (and g2rz g2ro)))
	  (pushnew (times+l (negate g1r) (negate g2r)) lcan :test #'eqlu))
	(when (and (or (and g1ln (not g1lz)) (and g1lz (not g1lo)))
		   (or (and (not g2rn) (not g2rz)) (and g2rz (not g2ro))))
	  (pushnew (negate (times+r (negate g1l) g2r)) lcan :test #'eqlu))
	(when (and (or (and (not g1rn) (not g1rz)) (and g1rz (not g1ro)))
		   (or (and g2ln (not g2lz)) (and g2lz (not g2lo))))
	  (pushnew (negate (times+r g1r (negate g2l))) lcan :test #'eqlu))
	(when (and (or (and (not g1rn) (not g1rz)) (and g1rz (not g1ro)))
		   (or (and (not g2rn) (not g2rz)) (and g2rz (not g2ro))))
	  (pushnew (times+r g1r g2r) rcan :test #'eqlu))
	(when (and (or (and g1ln (not g1lz)) (and g1lz (not g1lo)))
		   (or (and g2ln (not g2lz)) (and g2lz (not g2lo))))
	  (pushnew (times+r (negate g1l) (negate g2l)) rcan :test #'eqlu))
	(when (and (or (and g1rn (not g1rz)) (and g1rz g1ro))
		   (not g2ln))
	  (pushnew (negate (times+l (negate g1r) g2l)) rcan :test #'eqlu))
	(when (and (not g1ln)
		   (or (and g2rn (not g2rz)) (and g2rz g2ro)))
	  (pushnew (negate (times+l g1l (negate g2r))) rcan :test #'eqlu))
	(setf lcan (sort lcan #'lessp))
	(setf rcan (sort rcan #'lessp))
	(if (or (some #'nan-p lcan) (some #'nan-p rcan))
	    (make-instance 'gbound :nan-p t)
	    (let ((l* (car lcan))
		  (r* (car (last rcan))))
	      (make-gbound
	       (if (and (> (length lcan) 1)
			(let ((l2 (cadr lcan)))
			  (and (eql (to-rational l*) (to-rational l2))
			       (not (and (open-p l*) (open-p l2))))))
		   (copy-gnum l* :open-p nil)
		   l*)
	       (if (and (> (length rcan) 1)
			(let ((r2 (car (last rcan 2))))
			  (and (eql (to-rational r*) (to-rational r2))
			       (not (and (open-p r*) (open-p r2))))))
		   (copy-gnum r* :open-p nil)
		   r*)))))))

(defmethod times2 ((ub1 ubound) (ub2 ubound))
  (ubound (times2 (gbound ub1) (gbound ub2))))

(defmethod times ((ub1 ubound) (ub2 ubound) &rest ubs)
  (reduce #'times2 ubs :initial-value (times2 ub1 ub2)))

(defmethod times ((f1 real) (f2 real) &rest fs)
  (apply #'times (ubound f1) (ubound f2) (mapcar #'ubound fs)))

(defun divide+l (g1l g2l)
  (let* ((g1lz (unless (inf-p g1l) (eql 0 (a-frac g1l))))
	 (g1li (inf-p g1l))
	 (g1lo (open-p g1l))
	 (g2lz (unless (inf-p g2l) (eql 0 (a-frac g2l))))
	 (g2li (inf-p g2l))
	 (g2lo (open-p g2l)))
    (cond
      ((and g2lz (not g2lo)) (make-gnum :nan-p t :open-p t))
      ((and g1li (not g1lo))
       (if (and g2li (not g2lo))
	   (make-gnum :nan-p t :open-p t)
	   g2l))
      ((or (and g1lz (not g1lo)) (and g2li (not g2lo))) (make-gnum))
      ((or (and g1lz g1lo) (and g2li g2lo)) (make-gnum :open-p t))
      (t (let ((tmp (gnum (/ (to-rational g1l) (to-rational g2l)))))
	   (copy-gnum tmp :open-p (or g1lo g2lo)))))))

(defun divide+r (g1r g2r)
  (let* ((g1rz (unless (inf-p g1r) (eql 0 (a-frac g1r))))
	 (g1ri (inf-p g1r))
	 (g1ro (open-p g1r))
	 (g2rz (unless (inf-p g2r) (eql 0 (a-frac g2r))))
	 (g2ri (inf-p g2r))
	 (g2ro (open-p g2r)))
    (cond
      ((and g2rz (not g2ro)) (make-gnum :nan-p t :open-p t))
      ((and g1ri (not g1ro))
       (if (and g2ri (not g2ro))
	   (make-gnum :nan-p t :open-p t)
	   g2r))
      ((or (and g1rz (not g1ro)) (and g2ri (not g2ro))) (make-gnum))
      ((or (and g1ri g1ro) (and g2rz g2ro)) (make-gnum :inf-p t :open-p t))
      (t (let ((tmp (gnum (/ (to-rational g1r) (to-rational g2r)))))
	   (copy-gnum tmp :open-p (or g1ro g2ro)))))))

(defmethod divide2 ((gb1 gbound) (gb2 gbound))
  (if (or (nan-p gb1) (nan-p gb2))
      (make-instance 'gbound :nan-p t)
      (let* (lcan
	     rcan
	     (g1l (lo gb1))
	     (g1lz (unless (inf-p g1l) (eql 0 (a-frac g1l))))
	     (g1ln (eq (f-sign g1l) '-))
	     (g1lo (open-p g1l))
	     (g1r (hi gb1))
	     (g1rz (unless (inf-p g1r) (eql 0 (a-frac g1r))))
	     (g1rn (eq (f-sign g1r) '-))
	     (g1ro (open-p g1r))
	     (g2l (lo gb2))
	     (g2lz (unless (inf-p g2l) (eql 0 (a-frac g2l))))
	     (g2ln (eq (f-sign g2l) '-))
	     (g2lo (open-p g2l))
	     (g2r (hi gb2))
	     (g2rz (unless (inf-p g2r) (eql 0 (a-frac g2r))))
	     (g2rn (eq (f-sign g2r) '-))
	     (g2ro (open-p g2r)))
	(cond
	  ((and (or g2ln (and g2lz (not g2lo)))
		(or (not g2rn) (and g2rz (not g2ro))))
	   (make-instance 'gbound :nan-p t))
	  (t (when (and (not g1ln)
			(or (and (not g2rn) (not g2rz)) (and g2rz (not g2ro))))
	       (pushnew (divide+l g1l g2r) lcan :test #'eqlu))
	     (when (and (or (and g1rn (not g2rz)) (and g1rz g1ro))
			(or (and g2ln (not g2lz)) (and g2lz (not g2lo))))
	       (pushnew (divide+l (negate g1r) (negate g2l)) lcan
			:test #'eqlu))
	     (when (and (or (and g1ln (not g1lz)) (and g1lz (not g1lo)))
			(not g2ln))
	       (pushnew (negate (divide+r (negate g1l) g2l)) lcan
			:test #'eqlu))
	     (when (and (or (and (not g1rn) (not g1rz)) (and g1rz (not g1ro)))
			(or (and g2rn (not g2rz)) (and g2rz g2ro)))
	       (pushnew (negate (divide+r g1r (negate g2r))) lcan
			:test #'eqlu))
	     (when (and (or (and (not g1rn) (not g1rz)) (and g1rz (not g1ro)))
			(not g2ln))
	       (pushnew (divide+r g1r g2l) rcan :test #'eqlu))
	     (when (and (or (and g1ln (not g1lz)) (and g1lz (not g1lo)))
			(or (and g2rn (not g2rz)) (and g2rz g2ro)))
	       (pushnew (divide+r (negate g1l) (negate g2r)) rcan
			:test #'eqlu))
	     (when (and (or (and g1rn (not g1rz)) (and g1rz g1ro))
			(or (and (not g2rn) (not g2rz)) (and g2rz (not g2ro))))
	       (pushnew (negate (divide+l (negate g1r) g2r)) rcan
			:test #'eqlu))
	     (when (and (not g1ln)
			(or (and g2ln (not g2lz)) (and g2lz (not g2lo))))
	       (pushnew (negate (divide+l g1l (negate g2l))) rcan
			:test #'eqlu))
	     (setf lcan (sort lcan #'lessp))
	     (setf rcan (sort rcan #'lessp))
	     (if (or (some #'nan-p lcan) (some #'nan-p rcan))
		 (make-instance 'gbound :nan-p t)
		 (let ((l* (car lcan))
		       (r* (car (last rcan))))
		   (make-gbound
		    (if (and (> (length lcan) 1)
			     (let ((l2 (cadr lcan)))
			       (and (eql (to-rational l*) (to-rational l2))
				    (not (and (open-p l*) (open-p l2))))))
			(copy-gnum l* :open-p nil)
			l*)
		    (if (and (> (length rcan) 1)
			     (let ((r2 (car (last rcan 2))))
			       (and (eql (to-rational r*) (to-rational r2))
				    (not (and (open-p r*) (open-p r2))))))
			(copy-gnum r* :open-p nil)
			r*)))))))))

(defmethod divide2 ((ub1 ubound) (ub2 ubound))
  (ubound (divide2 (gbound ub1) (gbound ub2))))

(defmethod divide ((ub1 ubound) (ub2 ubound) &rest ubs)
  (reduce #'divide2 ubs :initial-value (divide2 ub1 ub2)))

(defmethod divide ((f1 real) (f2 real) &rest fs)
  (apply #'divide (ubound f1) (ubound f2) (mapcar #'ubound fs)))

(defmethod fma ((gb1 gbound) (gb2 gbound) (gb3 gbound))
  (plus2 (times2 gb1 gb2) gb3))

(defmethod fma ((ub1 ubound) (ub2 ubound) (ub3 ubound))
  (ubound (fma (gbound ub1) (gbound ub2) (gbound ub3))))

(defmethod fma ((f1 real) (f2 real) (f3 real))
  (fma (ubound f1) (ubound f2) (ubound f3)))

(defmethod fam ((gb1 gbound) (gb2 gbound) (gb3 gbound))
  (times2 (plus2 gb1 gb2) gb3))

(defmethod fam ((ub1 ubound) (ub2 ubound) (ub3 ubound))
  (ubound (fam (gbound ub1) (gbound ub2) (gbound ub3))))

(defmethod fam ((f1 real) (f2 real) (f3 real))
  (fam (ubound f1) (ubound f2) (ubound f3)))

(defmethod fdot ((gbv1 gbarray) (gbv2 gbarray))
  (assert (eql (length (arr gbv1)) (length (arr gbv2))))
  (loop with s = (make-gbound (make-gnum) (make-gnum))
     for gb1 across (arr gbv1)
     and gb2 across (arr gbv2)
     do (setf s (plus2 s (times2 gb1 gb2)))
     finally (return s)))

(defmethod fdot ((ubv1 ubarray) (ubv2 ubarray))
  (ubound
   (fdot (make-gbarray (map 'vector #'gbound (arr ubv1)))
	 (make-gbarray (map 'vector #'gbound (arr ubv2))))))

(defmethod fdot ((v1 vector) (v2 vector))
  (fdot (make-ubarray (map 'vector #'ubound v1))
	(make-ubarray (map 'vector #'ubound v2))))

(defmethod fsum ((gbv gbarray))
  (loop with s = (make-gbound (make-gnum) (make-gnum))
     for gb across (arr gbv)
     do (setf s (plus2 s gb))
     finally (return s)))

(defmethod fsum ((ubv ubarray))
  (ubound (fsum (make-gbarray (map 'vector #'gbound (arr ubv))))))

(defmethod fsum ((v vector))
  (fsum (make-ubarray (map 'vector #'ubound v))))

(defmethod fprod ((gbv gbarray))
  (loop with s = (make-gbound (make-gnum :a-frac 1) (make-gnum :a-frac 1))
     for gb across (arr gbv)
     do (setf s (times2 s gb))
     finally (return s)))

(defmethod fprod ((ubv ubarray))
  (ubound (fprod (make-gbarray (map 'vector #'gbound (arr ubv))))))

(defmethod fprod ((v vector))
  (fprod (make-ubarray (map 'vector #'ubound v))))

(defmethod fprod-ratio ((gbv1 gbarray) (gbv2 gbarray))
  (divide2 (fprod gbv1) (fprod gbv2)))

(defmethod fprod-ratio ((ubv1 ubarray) (ubv2 ubarray))
  (ubound
   (fprod-ratio (make-gbarray (map 'vector #'gbound (arr ubv1)))
		(make-gbarray (map 'vector #'gbound (arr ubv2))))))

(defmethod fprod-ratio ((v1 vector) (v2 vector))
  (fprod-ratio (make-ubarray (map 'vector #'ubound v1))
	       (make-ubarray (map 'vector #'ubound v2))))

(defmethod square ((gb gbound))
  (cond
    ((nan-p gb) gb)
    (t (let* ((gl (lo gb))
	      (gr (hi gb))
	      (rl (to-rational gl))
	      (rr (to-rational gr))
	      (s1 (* rl rl))
	      (s2 (* rr rr))
	      (g1 (gnum s1 :open-p (open-p gl)))
	      (g2 (gnum s2 :open-p (open-p gr)))
	      (gmin (if (< s1 s2) g1 g2))
	      (gmax (if (> s1 s2) g1 g2)))
	 (cond
	   ((or (and (< rl 0) (> rr 0)) (and (< rr 0) (> rl 0))
		(and (eql 0 rl) (exactp gl)) (and (eql 0 rr) (exactp gr)))
	    (if (eql s1 s2)
		(make-gbound
		 (make-gnum)
		 (copy-gnum g1 :open-p (and (open-p g1) (open-p g2))))
		(make-gbound (make-gnum) gmax)))
	   (t (make-gbound gmin gmax)))))))

(defmethod square ((ub ubound))
  (ubound (square (gbound ub))))

(defmethod square ((f real))
  (square (ubound f)))

(defmethod sqroot ((gb gbound) &optional (proto 1.0d0))
  (cond
    ((nan-p gb) gb)
    ((< (to-rational (lo gb)) 0) (make-instance 'gbound :nan-p t))
    (t (let* ((gl (lo gb))
	      (gl* (to-rational gl))
	      (gr (hi gb))
	      (gr* (to-rational gr))
	      (sl (sqrt (float gl* proto)))
	      (sl* (or (ignore-errors (rational sl)) sl))
	      (sr (sqrt (float gr* proto)))
	      (sr* (or (ignore-errors (rational sr)) sr)))
	 (cond
	   ((eql gl* gr*) ; careful with (ubound #gb[gs, gs])
	    (let ((gs (gnum sl* :open-p (or (open-p gl) (open-p gr)))))
	      (make-gbound gs gs)))
	   (t (make-gbound
	       (gnum
		sl*
		:open-p
		(or (open-p gl)
		    (not (eql sl* (or (ignore-errors (rationalize sl)) sl)))))
	       (gnum
		sr*
		:open-p
		(or
		 (open-p gr)
		 (not
		  (eql sr* (or (ignore-errors (rationalize sr)) sr))))))))))))

(defmethod sqroot ((ub ubound) &optional (proto 1.0d0))
  (ubound (sqroot (gbound ub) proto)))

(defmethod sqroot ((f real) &optional (proto 1.0d0))
  (sqroot (ubound f) proto))

(defun power+l (gxl gyl)
  (let ((xr (to-rational gxl))
	(yr (to-rational gyl)))
    (cond
      ((and (eql xr 1) (exactp gxl))
       (if (and (inf-p gyl) (exactp gyl))
	   (make-gnum :nan-p t :open-p t)
	   gxl))
      ((and (eql yr 0) (exactp gyl))
       (if (and (inf-p gxl) (exactp gxl))
	   (make-gnum :nan-p t :open-p t)
	   (make-gnum :a-frac 1)))
      ((and (eql xr 1) (open-p gxl))
       (if (and (inf-p gyl) (exactp gyl))
	   gyl
	   gxl))
      ((and (eql yr 0) (open-p gyl))
       (if (and (inf-p gxl) (exactp gxl))
	   gxl
	   (make-gnum :a-frac 1 :open-p t)))
      ((and (inf-p gxl) (exactp gxl)) gxl)
      ((and (inf-p gyl) (exactp gyl)) gyl)
      (t (make-gnum :a-frac (expt xr yr)
		    :open-p (or (open-p gxl) (open-p gyl)))))))

(defun power+r (gxr gyr)
  (let ((xr (to-rational gxr))
	(yr (to-rational gyr)))
    (cond
      ((and (inf-p gxr) (exactp gxr))
       (if (and (eql yr 0) (exactp gyr))
	   (make-gnum :nan-p t :open-p t)
	   gxr))
      ((and (inf-p gyr) (exactp gyr))
       (if (and (eql xr 1) (exactp gxr))
	   (make-gnum :nan-p t :open-p t)
	   gyr))
      ((and (inf-p gxr) (open-p gxr))
       (if (and (eql yr 0) (exactp gyr))
	   (make-gnum :a-frac 1)
	   gxr))
      ((and (inf-p gyr) (open-p gyr))
       (if (and (eql xr 1) (exactp gxr))
	   gxr
	   gyr))
      ((and (eql xr 1) (exactp gxr)) gxr)
      ((and (eql yr 0) (exactp gyr)) (make-gnum :a-frac 1))
      (t (make-gnum :a-frac (expt xr yr)
		    :open-p (or (open-p gxr) (open-p gyr)))))))

(defun reciprocate (gx)
  (cond
    ((nan-p gx) gx)
    ((eql 0 (to-rational gx)) (make-gnum :inf-p t :open-p (open-p gx)))
    (t (make-gnum :a-frac (/ 1 (to-rational gx)) :open-p (open-p gx)))))

(defmethod power ((gbx gbound) (gby gbound))
  (if (or (nan-p gbx) (nan-p gby))
      (make-instance 'gbound :nan-p t)
      (let* (lcan
	     rcan
	     (gxl (lo gbx))
	     (gxli (inf-p gxl))
	     (gxlz (unless gxli (eql 0 (a-frac gxl))))
	     (gxln (eq (f-sign gxl) '-))
	     (gxlo (open-p gxl))
	     (xl (to-rational gxl))
	     (gxr (hi gbx))
	     (gxri (inf-p gxr))
	     (gxrz (unless gxri (eql 0 (a-frac gxr))))
	     (gxrn (eq (f-sign gxr) '-))
	     (gxro (open-p gxr))
	     (xr (to-rational gxr))
	     (gyl (lo gby))
	     (gyli (inf-p gyl))
	     (gylz (unless gyli (eql 0 (a-frac gyl))))
	     (gyln (eq (f-sign gyl) '-))
	     (gylo (open-p gyl))
	     (yl (to-rational gyl))
	     (gyr (hi gby))
	     (gyri (inf-p gyr))
	     (gyrz (unless gyri (eql 0 (a-frac gyr))))
	     (gyrn (eq (f-sign gyr) '-))
	     (gyro (open-p gyr))
	     (yr (to-rational gyr)))
	(cond
	  ((and (or (and gxln (not gxlz)) (and gxlz (not gxlo)))
		(or (and (not gxrn) (not gxrz)) (and gxrz (not gxro)))
		(or (and gyln (not gylz)) (and gylz (not gylo)))
		(not (and (eql yl yr) gyln (not gylz)
			  (and (integerp yl) (evenp yl)))))
	   (make-instance 'gbound :nan-p t))
	  ((and gyri gyrn (not gyro)
		(or (> xl 1) (and (eql xl 0) gxlo)
		    (< xr 1) (and (eql xr 0) gxro)))
		(make-gbound (make-gnum) (make-gnum)))
	  ((and (eql yl yr) (not gylo) (not gyro) (integerp yl))
	   (cond
	     (gylz
	      (if (or (and (or (and gxln (not gxlz)) (and gxlz gxlo))
			   (implies (and gxri (not gxrn)) gxro))
		      (and (or (and gxrn (not gxrz)) (and gxrz gxro))
			   (implies (and gxli gxln) gxlo)))
		  (make-gbound (make-gnum :a-frac 1) (make-gnum :a-frac 1))
		  (make-instance 'gbound :nan-p t)))
	     ((and (evenp yl) (not gyln))
	      (cond
		((or (and gxrn (not gxrz)) (and gxrz gxro))
		 (make-gbound (gnum (expt xr yl) :open-p gxro)
			      (gnum (expt xl yl) :open-p gxlo)))
		((or (and (not gxln) (not gxlz)) (and gxlz gxlo))
		 (make-gbound (gnum (expt xl yl) :open-p gxlo)
			      (gnum (expt xr yl) :open-p gxro)))
		(t (let ((t1 (expt xl yl))
			 (t2 (expt xr yl)))
		     (cond
		       ((< t1 t2)
			(make-gbound
			 (make-gnum) (gnum t2 :open-p gxro)))
		       ((> t1 t2)
			(make-gbound
			 (make-gnum) (gnum t1 :open-p gxlo)))
		       (t (make-gbound
			   (make-gnum)
			   (gnum t1 :open-p (or gxlo gxro)))))))))
	     ((and (evenp yl) gyln)
	      (cond
		((or (and (not gxln) (not gxlz)) (and gxlz gxlo))
		 (make-gbound
		  (gnum (expt xr yl) :open-p gxro)
		  (if gxlz
		      (make-gnum :inf-p t :open-p gxlo)
		      (gnum (expt xl yl) :open-p gxlo))))
		((or (and gxrn (not gxrz)) (and gxrz gxro))
		 (make-gbound
		  (gnum (expt xl yl) :open-p gxlo)
		  (if gxrz
		      (make-gnum :inf-p t :open-p gxlo)
		      (gnum (expt xr yl) :open-p gxlo))))
		(t (let* ((c (and gxlz gyln (not gylz)))
			  (t1 (if c (lisp-inf 1.0d0) (expt xl yl)))
			  (t2 (if c (lisp-inf 1.0d0) (expt xr yl))))
		     (cond
		       ((eql t1 t2)
			(make-gbound
			 (gnum t1 :open-p (or gxlo gxro))
			 (make-gnum :inf-p t)))
		       (t (make-gbound
			   (gnum (min t1 t2) :open-p (if (> t1 t2) gxro gxlo))
			   (make-gnum :inf-p t))))))))
	     ((not gyln)
	      (make-gbound (gnum (expt xl yl) :open-p gxlo)
			   (gnum (expt xr yl) :open-p gxro)))
	     (t (make-gbound
		 (if gxrz
		     (make-gnum :inf-p t :f-sign '- :open-p gxro)
		     (gnum (expt xr yl) :open-p gxro))
		 (if gxlz
		     (make-gnum :inf-p t :open-p gxro) ; gxlo? (x2)
		     (gnum (expt xl yl) :open-p gxro))))))
	  ((and gxln (not gxlz)) (make-instance 'gbound :nan-p t))
	  (t (when (and (>= xl 1) (not gyln))
	       (pushnew (power+l gxl gyl) lcan :test #'eqlu))
	     (when (and (or (< xr 1) (and (eql xr 1) gxro))
			(or (and gyrn (not gyrz)) (and gyrz gyro)))
	       (pushnew
		(power+l (reciprocate gxr) (negate gyr)) lcan :test #'eqlu))
	     (when (and (or (< xl 1) (and (eql xl 1) (not gxlo)))
			(or (and (not gyrn) (not gyrz)) (and gyrz (not gyro))))
	       (pushnew
		(reciprocate (power+r (reciprocate gxl) gyr)) lcan
		:test #'eqlu))
	     (when (and (or (> xr 1) (and (eql xr 1) (not gxro)))
			(or (and gyln (not gylz)) (and gylz (not gylo))))
	       (pushnew
		(reciprocate (power+r gxr (negate gyl))) lcan :test #'eqlu))
	     (when (and (or (> xr 1) (and (eql xr 1) (not gxro)))
			(or (and (not gyrn) (not gyrz)) (and gyrz (not gyro))))
	       (pushnew	(power+r gxr gyr) rcan :test #'eqlu))
	     (when (and (or (< xl 1) (and (eql xl 1) (not gxlo)))
			(or (and gyln (not gylz)) (and gylz (not gylo))))
	       (pushnew
		(power+r (reciprocate gxl) (negate gyl)) rcan :test #'eqlu))
	     (when (and (or (< xl 1) (and (eql xl 1) gxlo)) (not gyln))
	       (pushnew
		(reciprocate (power+l (reciprocate gxr) gyl)) rcan
		:test #'eqlu))
	     (when (and (>= xl 1)
			(or (and gyrn (not gyrz)) (and gyrz gyro)))
	       (pushnew
		(reciprocate (power+l gxl (negate gyr))) rcan :test #'eqlu))
	     (setf lcan (sort lcan #'lessp))
	     (setf rcan (sort rcan #'lessp))
	     (if (or (some #'nan-p lcan) (some #'nan-p rcan))
		 (make-instance 'gbound :nan-p t)
		 (let ((l* (car lcan))
		       (r* (car (last rcan))))
		   (make-gbound
		    (if (and (> (length lcan) 1)
			     (let ((l2 (cadr lcan)))
			       (and (eql (to-rational l*) (to-rational l2))
				    (not (and (open-p l*) (open-p l2))))))
			(copy-gnum l* :open-p nil)
			l*)
		    (if (and (> (length rcan) 1)
			     (let ((r2 (car (last rcan 2))))
			       (and (eql (to-rational r*) (to-rational r2))
				    (not (and (open-p r*) (open-p r2))))))
			(copy-gnum r* :open-p nil)
			r*)))))))))

(defmethod power ((ubx ubound) (uby ubound))
  (ubound (power (gbound ubx) (gbound uby))))

(defmethod power ((x real) (y real))
  (power (ubound x) (ubound y)))

(defmethod e-power ((gby gbound) &optional (proto 1.0d0))
  (flet ((%exp (g)
	   (cond
	     ((and (inf-p g) (eq (f-sign g) '-) (not (open-p g))) (make-gnum))
	     ((and (eql (a-frac g) 0) (not (open-p g))) (gnum 1))
	     ((and (inf-p g) (eq (f-sign g) '+) (not (open-p g)))
	      (make-gnum :inf-p t))
	     ((< (to-rational g) (log (min-rational *env*))) (gnum 0 :open-p t))
	     ((> (to-rational g) (log (max-rational *env*)))
	      (gnum (max-rational *env*) :open-p t))
	     (t (gnum (exp (float (to-rational g) proto)) :open-p t)))))
    (cond
      ((nan-p gby) gby)
      (t (let ((gyl (lo gby))
	       (gyh (hi gby)))
	   (cond
	     ((eql (to-rational gyl) (to-rational gyh)) ; open-p?
	      (let ((gr (gnum (exp (float (to-rational gyl) proto))
			      :open-p nil)))
		(make-gbound gr gr)))
	     (t (make-gbound (%exp gyl) (%exp gyh)))))))))

(defmethod e-power ((uby ubound) &optional (proto 1.0d0))
  (ubound (e-power (gbound uby) proto)))

(defmethod e-power ((y real) &optional (proto 1.0d0))
  (e-power (ubound y) proto))

(defmethod intersect ((gb1 gbound) (gb2 gbound))
  (if (or (nan-p gb1) (nan-p gb2))
      (gbound-nan)
      (let* ((g1l (lo gb1))
	     (g1r (hi gb1))
	     (g2l (lo gb2))
	     (g2r (hi gb2))
	     (g1lv (to-rational g1l))
	     (g1rv (to-rational g1r))
	     (g2lv (to-rational g2l))
	     (g2rv (to-rational g2r)))
	(cond
	  ((or (< g1lv g2lv) (and (eql g1lv g2lv) (open-p g2l)))
	   (cond
	     ((or (< g1rv g2lv)
		  (and (eql g1rv g2lv) (or (open-p g1r) (open-p g2l))))
	      (gbound-nan))
	     ((or (< g1rv g2rv)
		  (and (eql g1rv g2rv) (or (open-p g1r) (not (open-p g2r)))))
	      (make-gbound g2l g1r))
	     (t gb2)))
	  ((or (< g1lv g2rv)
	       (and (eql g1lv g2rv) (not (or (open-p g1l) (open-p g2r)))))
	   (if (or (< g1rv g2rv)
		   (and (eql g1rv g2rv) (or (open-p g1r) (not (open-p g2r)))))
	       gb1
	       (make-gbound g1l g2r)))
	  (t (gbound-nan))))))

(defmethod intersect ((ub1 ubound) (ub2 ubound))
  (ubound (intersect (gbound ub1) (gbound ub2))))

(defun binomial (n m)
  (assert (<= m n))
  (loop for r = 1 then (* r (/ i j))
     for i downfrom n
     and j downfrom m downto 1
     finally (return r)))

(defmethod poly-taylor ((cs gbarray) (x gbound) (x0 gbound))
  (if (or (and (inf-p (lo x0)) (eq (f-sign (lo x0)) '-))
	  (and (inf-p (hi x0)) (eq (f-sign (hi x0)) '+)))
      (make-gbound (gnum (lisp-inf -1.0d0)) (gnum (lisp-inf 1.0d0)))
      (let ((k (length (arr cs)))
	    (cts (make-gbarray (copy-seq (arr cs)))))
	(loop with xm = (minus2 x x0)
	   for j from 0 to (1- k)
	   do (loop for i downfrom (- k 1) downto j
		 for b = (gnum (binomial i j))
		 for bb = (make-gbound b b)
		 for p = (times2 (uref cs i) bb) then
		   (plus2 (times2 (uref cs i) bb) (times2 x0 p))
		 finally (setf (uref cts j) p))
	   finally (return (loop for i downfrom k downto 1
			      for p = (uref cts (1- i)) then
				(plus2 (uref cts (1- i)) (times2 xm p))
			      finally (return p)))))))

(defmethod poly-inexact ((cs gbarray) (x gbound))
  (let ((x0l (copy-gnum (lo x) :open-p nil))
  	(x0r (copy-gnum (hi x) :open-p nil)))
    (intersect
     (poly-taylor cs x (make-gbound x0l x0l))
     (poly-taylor cs x (make-gbound x0r x0r)))))

(defmethod poly-exact ((cs gbarray) (x gbound))
  (loop with k = (length (arr cs))
     for i downfrom k downto 1
     for p = (uref cs (1- k)) then (plus2 (uref cs (1- i)) (times2 p x))
     finally (return p)))

(defmethod poly-exact* ((cs gbarray) (x gbound))
  (assert (eq (open-p (lo x)) (open-p (hi x))))
  (let* ((op (open-p (lo x)))
	 (y (if op
		(make-gbound (copy-gnum (lo x) :open-p nil)
			     (copy-gnum (hi x) :open-p nil))
		x))
	 (p (poly-exact cs y)))
    (if op
	(make-gbound (copy-gnum (lo p) :open-p t)
		     (copy-gnum (hi p) :open-p t))
	p)))

(defmethod gugle ((gb gbound)) ; TODO: generalize
  ;; tallies data out-of and back-into scratchpad (g-u-g)
  (gbound (ubound gb)))

(defmethod poly ((cs gbarray) (x gbound))
  (cond
    ((or (nan-p x) (some #'nan-p (arr cs))) (make-instance 'gbound :nan-p t))
    ((equalu (lo x) (hi x)) (poly-exact* cs x)) ; NOTE: open-p's must equal
    ((eql 1 (length (arr cs))) (uref cs 0))
    ((eql 2 (length (arr cs))) (fma (uref cs 1) x (uref cs 0)))
    (t (let* ((l (poly-exact* cs (make-gbound (lo x) (lo x))))
	      (r (poly-exact* cs (make-gbound (hi x) (hi x))))
	      (min (if (lessp (lo l) (lo r)) (lo l) (lo r)))
	      (max (if (greaterp (hi l) (hi r)) (hi l) (hi r))))
	 (loop with ts = (list x)
	    while ts
	    for trial = (pop ts)
	    for p = (poly-inexact cs trial)
	    for p2 = (gugle p)
	    for b = (gugle (make-gbound min max))
	    do (unless (eqlu (intersect p2 b) p2)
		 (setf ts (nconc (bisect trial) ts))
		 (let* ((x2 (copy-gnum (hi (car ts)) :open-p nil))
			(p3 (poly-exact cs (make-gbound x2 x2)))
			(b2 (gugle
			     (make-gbound
			      (if (lessp (lo p3) min) (lo p3) min)
			      (if (greaterp (hi p3) max) (hi p3) max)))))
		   (setf min (lo b2)
			 max (hi b2))))
	    finally (return (make-gbound min max)))))))

(defmethod poly ((cs ubarray) (x ubound))
  (ubound (poly (make-gbarray (map 'vector #'gbound (arr cs))) (gbound x))))

(defmethod poly ((cs vector) (x real))
  (poly (make-ubarray (map 'vector #'ubound cs)) (ubound x)))

(defmethod absolute ((x gnum))
  (cond
    ((nan-p x) x)
    ((lessp x (gnum 0)) (copy-gnum x :f-sign '+))
    (t x)))

(defmethod absolute ((x gbound))
  (if (nan-p x)
      x
      (let ((z (gnum 0))
	    (l (lo x))
	    (r (hi x)))
	(cond
	  ((not (greaterp r z)) (make-gbound (absolute r) (absolute l)))
	  ((not (greaterp l z))
	   (let ((al (absolute l))
		 (ar (absolute r)))
	     (cond
	       ((lessp al ar) (make-gbound z ar))
	       ((greaterp al ar) (make-gbound z al))
	       (t (make-gbound
		   z (copy-gnum r :open-p (and (open-p l) (open-p r))))))))
	  (t x)))))

(defmethod absolute ((x ubound))
  (ubound (absolute (gbound x))))

(defmethod absolute ((x real))
  (absolute (ubound x)))

#+cover-unum (cover:annotate nil)

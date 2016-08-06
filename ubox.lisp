;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2016 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

#+cover-unum (cover:annotate t)

(in-package :unum)

(defmethod ulp-hi ((u unum))
  (assert (exactp u))
  (multiple-value-bind (ul ur)
      (widen-to-ulp* (if (eq (sign u) '+) (inc-ubit u) (dec-ubit u)) :ubit 1)
    (- (to-rational ur) (to-rational ul))))

(defmethod ulp-lo ((u unum))
  (ulp-hi (negate u)))

(defun dec*-f-size (u)
  (loop for fut = (dec-f-size u)
     while (and (> (f-size u) 1) (exactp fut))
     do (setf u fut)
     finally (return u)))

(defmethod favor-e ((u unum))
  (let ((u (dec-f-size (inc-e-size u))))
    (if (or (inexactp u) (eql (e-size u) (max-e-size u)))
	u
	(dec*-f-size u))))

(defmethod pad-frac ((u unum))
  (let ((f (frac u))
	(mfs (max-f-size u)))
    (copy-unum u :frac (ash f (- mfs (integer-length f))) :f-size mfs)))

(defmethod favor-f ((u unum))
  (let ((u (dec-e-size (pad-frac u))))
    (if (inexactp u)
	u
	(dec*-f-size u))))

(defmethod nbor-hi ((u unum) minp)
  (let* ((ut (if (and (eql 0 (to-rational u)) (exactp u)) (make-unum) u))
	 (s (to-signum (sign ut)))
	 (minu (small-subnormal *env*))
	 (maxu (max-unum *env*))
	 oflow ulpminu)
    (if (< minp (log (to-rational minu) 2))
	(setf ulpminu minu)
	(if (> minp (log (to-rational maxu) 2))
	    (setf oflow t
		  ulpminu (unum (expt 2 (floor (log (to-rational maxu) 2)))))
	    (setf ulpminu (unum (expt 2 minp)))))
    (cond
      ((or (nan-p u) (eqlu u (unum-inf *env*))) (unum-nan *env*))
      ((eqlu u (unum-inf *env* :sign '-))
       (if oflow
	   (if (and (eql 0 (ess *env*)) (eql 0 (fss *env*)))
	       (copy-unum (unum -2) :ubit 1)
	       (copy-unum (unum -3) :ubit 1))
	   (copy-unum (max-unum *env* :sign '-) :ubit 1)))
      ((inexactp u)
       (multiple-value-bind (u* u+ulp) (widen-to-ulp* u)
	 (declare (ignore u*))
	 u+ulp))
      ((and oflow (eqlu u (unum 2)) (eql 0 (ess *env*)) (eql 0 (fss *env*)))
       (copy-unum (unum 2) :ubit 1))
      ((and oflow (eqlu u (unum 3))
	    (not (and (eql 0 (ess *env*)) (eql 0 (fss *env*)))))
       (copy-unum (unum 3) :ubit 1))
      (t (let* ((rt (to-rational ut))
		(ulpmin (to-rational ulpminu))
		(mes (max-e-size u))
		(mfs (max-f-size u)))
	   (loop until (integerp (/ rt ulpmin))
	      do (setf ulpmin (/ ulpmin 2)))
	   (loop for fut = (favor-f ut)
	      while (and (< (ulp-hi ut) ulpmin)
			 (not (eqlu ut fut)) (exactp fut))
	      do (setf ut fut))
	   (loop for eut = (inc-e-size ut)
	      while (and (< (e-size ut) mes) (>= (ulp-hi eut) ulpmin))
	      do (setf ut eut))
	   (loop for fut = (inc-f-size ut)
	      while (and (< (f-size ut) mfs) (>= (ulp-hi fut) ulpmin))
	      do (setf ut fut))
	   (if (> s 0) (inc-ubit ut) (dec-ubit ut)))))))

(defmethod nbor-lo ((u unum) minp)
  (cond
    ((and (eql 0 (to-rational u)) (exactp u)
	  (< minp (log (to-rational (small-subnormal *env*)) 2)))
     (almost-zero-unum *env* :sign '-))
    (t (negate (nbor-hi (negate u) minp)))))

(defun make-ubox (arr)
  (make-instance 'ubox :arr (coerce arr 'vector)))

(defmethod print-object ((ux ubox) s)
  (format s "#ubox[~<~{~A~^, ~:_~}~:>]" (list (coerce (arr ux) 'list))))

(defun combinations (&rest lists)
  (if (car lists)
      (mapcan (lambda (comb) (mapcar (lambda (n) (cons n comb)) (car lists)))
              (apply #'combinations (cdr lists)))
      (list nil)))

(defun find-nbors (uboxes minps)
  (loop
     with uns = nil
     with tmp = (make-array (length (arr (first uboxes))) :initial-element nil)
     for ux in uboxes do
       (loop for u across (arr ux)
	  and i from 0
	  and minp in minps
	  do (setf (aref tmp i)
		   (nconc
		    (aref tmp i)
		    (list (nbor-lo u minp) u (nbor-hi u minp))))
	  finally (setf uns (nconc
			     uns
			     (apply #'combinations (coerce tmp 'list)))
			tmp (make-array (length (arr (first uboxes)))
					:initial-element nil)))
     finally (return
	       (set-difference
		(remove-duplicates (mapcar #'make-ubox uns) :test #'eqlu)
		uboxes :test #'eqlu))))

(defmethod ubox-list ((ub ubound) minp)
  (assert (integerp minp))
  (if (or (nan-p ub) (inf-p ub))
      nil
      (let* ((ul (lo ub))
	     (ur (hi* ub))
	     (-inf (unum-inf ul :sign '-))
	     (+inf (unum-inf ul))
	     (ut (if (eqlu ul -inf)
		     -inf
		     (nbor-hi (nbor-lo ul minp) minp)))
	     (nt (nbor-hi ut minp))
	     l1 l2)
	(when (and (eqlu ut (almost-inf-unum ut))
		   (< (e-size ut) (max-e-size ut))
		   (let ((nb (ubound nt))) (disjointp (intersect ub nb) nb)))
	  (setf ut (copy-unum (inc-e-size (exact ut)) :ubit (ubit ut))))
	(loop for tb = (ubound ut)
	   while (and (equalu (intersect ub tb) tb) (not (eqlu ut +inf)))
	   do (push ut l1)
	     (setf ut (nbor-hi ut minp)
		   tb (ubound ut))
	   when (and (eqlu ut (almost-inf-unum ut))
		     (< (e-size ut) (max-e-size ut))
		     (disjointp (intersect ub tb) tb))
	   do (setf ut (copy-unum (inc-e-size (exact ut)) :ubit (ubit ut))))
	(setf ut (if (eqlu ur +inf) ur (nbor-lo (nbor-hi ur minp) minp)))
	(loop for tb = (ubound ut)
	   while (and (greaterp tb (if (null l1) (ubound ul) (ubound (car l1))))
		      (not (eqlu ut -inf)))
	   do (push ut l2)
	     (setf ut (nbor-lo ut minp)))
	(nconc (nreverse l1) l2))))

(defmethod inex-ubox-list ((ub ubound) minp)
  (loop for u in (ubox-list ub minp) when (inexactp u) collect u))

(defmethod split ((ub ubound))
  (without-scratchpad ()
    (if (nan-p ub)
	(list ub)
	(let* ((ul (lo ub))
	       (ur (hi* ub))
	       (gb (gbound ub))
	       (gl (lo gb))
	       (gr (hi gb))
	       (gl* (copy-gnum gl :open-p t))
	       (gr* (copy-gnum gr :open-p t)))
	  (cond
	    ((and (exactp gl) (exactp gr))
	     (if (equalu ul ur)
		 (list ub)
		 (list (ubound ul)
		       (ubound (make-gbound gl* gr*))
		       (ubound ur))))
	    ((and (inexactp gl) (exactp gr))
	     (list (ubound (make-gbound gl* gr*))
		   (ubound ur)))
	    ((and (exactp gl) (inexactp gr))
	     (list (ubound ul)
		   (ubound (make-gbound gl* gr*))))
	    ((and (inf-p gl) (eq '- (f-sign gl)))
	     (let ((-max (max-rational *env* :sign '-)))
	       (if (eql (to-rational gr) -max)
		   (list ub)
		   (list (ubound (almost-inf-unum *env* :sign '-))
			 (ubound (max-unum *env* :sign '-))
			 (ubound (make-gbound (gnum -max :open-p t) gr*))))))
	    ((and (inf-p gr) (eq '+ (f-sign gr)))
	     (let ((max (max-rational *env* :sign '+)))
	       (if (eql (to-rational gl) max)
		   (list ub)
		   (list (ubound (make-gbound gl* (gnum max :open-p t)))
			 (ubound (max-unum *env*))
			 (ubound (almost-inf-unum *env*))))))
	    (t (let* ((rl (to-rational gl))
		      (rr (to-rational gr))
		      (gm (gbound (unum (/ (+ rl rr) 2))))
		      (gml (lo gm))
		      (gmr (hi gm))
		      (gml* (copy-gnum gml :open-p t))
		      (gmr* (copy-gnum gmr :open-p t)))
		 (cond
		   ((greaterp (exact gml) (exact gl))
		    (list (ubound (make-gbound gl* gml*))
			  (ubound (unum gml))
			  (ubound (make-gbound gml* gr*))))
		   ((lessp (exact gmr) (exact gr))
		    (list (ubound (make-gbound gl* gmr*))
			  (ubound (unum gmr))
			  (ubound (make-gbound gmr* gr*))))
		   (t (list ub))))))))))

(defmethod bisect ((gb gbound))
  (let* ((lg (lo gb))
	 (lr (to-rational lg))
	 (rg (hi gb))
	 (rr (to-rational rg))
	 (mid
	  (cond
	    ((< lr 0 rr) 0)
	    ((eql lr (lisp-inf -1.0d0))
	     (if (> rr (max-rational *env* :sign '-))
		 (max-rational *env* :sign '-)
		 (return-from bisect nil))) ; ? (x2)
	    ((eql rr (lisp-inf 1.0d0))
	     (if (< lr (max-rational *env*))
		 (max-rational *env*)
		 (return-from bisect nil)))
	    (t (let ((m (expt 2 (floor (log (- rr lr) 2)))))
		 (cond
		   ((or (integerp (/ lr m)) (eql lr 0) (eql rr 0))
		    (if (< (/ (- rr lr) m) 2) ; precision issues...
			(/ (+ lr rr) 2)
			(* m (1+ (floor lr m)))))
		   (t (* m (ceiling lr m)))))))))
    (list (make-gbound lg (gnum mid :open-p t))
	  (make-gbound (gnum mid :open-p t) rg))))

(defmethod leftmost ((ub1 ubound) (ub2 ubound))
  (cond
    ((nan-p ub1) ub2)
    (t (let* ((r1 (to-real (lo ub1)))
	      (r2 (to-real (lo ub2)))
	      (r1* (lo r1))
	      (r2* (lo r2)))
	 (if (or (< r1* r2*) (and (eql r1* r2*) (rationalp r1)))
	     ub1
	     ub2)))))

(defmethod rightmost ((ub1 ubound) (ub2 ubound))
  (cond
    ((nan-p ub1) ub2)
    (t (let* ((r1 (to-real (hi* ub1)))
	      (r2 (to-real (hi* ub2)))
	      (r1* (hi* r1))
	      (r2* (hi* r2)))
	 (if (or (> r1* r2*) (and (eql r1* r2*) (rationalp r1)))
	     ub1
	     ub2)))))

(defmethod unite ((ub ubound) ubs)
  (cond
    ((null ubs) (list ub))
    (t (loop for ub2 in ubs
	  if (or (jointp ub ub2)
		 (jointp ub (ubound (nbor-lo (lo ub2) (lisp-inf -1.0d0))))
		 (jointp ub (ubound (nbor-hi (hi* ub2) (lisp-inf 1.0d0)))))
	  do (setf ub (make-ubound (lo (leftmost ub ub2))
				   (hi* (rightmost ub ub2))))
	  else collect ub2 into coallesced
	  finally (return (cons ub coallesced))))))

(defmethod insert ((ub ubound) ubs) ; ubs: sorted, disjoint, NaN-less
  (if (null ubs)
      (list ub)
      (loop for l = (length ubs)
	 for (ubi . ubr) on ubs
	 and i from 0
	 while (lessp ubi ub)
	 collect ubi into ubp
	 finally (let ((lt (when ubp
			     (jointp
			      ub
			      (ubound
			       (nbor-hi
				(hi* (car (last ubp))) (lisp-inf -1.0d0))))))
		       (rt (jointp
			    ub
			    (ubound (nbor-lo (lo ubi) (lisp-inf -1.0d0))))))
		   (return
		     (cond
		       ((and lt rt)
			(nconc
			 (butlast ubp)
			 (list
			  (make-ubound (lo (car (last ubp))) (hi* ubi))) ubr))
		       ((and lt (not rt))
			(nconc
			 (butlast ubp)
			 (list (make-ubound (lo (car (last ubp))) (hi* ub)))
			 (when (< 0 i) (list ubi))
			 ubr))
		       ((and (not lt) rt)
			(nconc
			 ubp (list (make-ubound (lo ub) (hi* ubi))) ubr))
		       (t (nconc
			   ubp (list ub) (when (< 0 i) (list ubi)) ubr))))))))

(defun solve-ulp (domain test-fn)
  (loop with trials = domain and sols = nil
     while trials
     do (loop with new = nil
	   for ub in trials
	   when (funcall test-fn ub)
	   do (let ((tmp (split ub)))
		(if (eql 1 (length tmp))
		    (setf sols (insert (car tmp) sols))
		    (setf new (nconc tmp new))))
	   finally (setf trials new))
     finally (return sols)))

#+cover-unum (cover:annotate t)

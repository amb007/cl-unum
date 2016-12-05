;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2016 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

#+cover-unum (cover:annotate t)

(in-package :unum)

(defmacro implies (a b)
  `(or (not ,a) ,b))

(defmacro erp (e)
  (let ((v (gensym)))
    `(let ((,v ,e))
       (format *terminal-io* "~&~<~2I~S =>~:_ ~S~:>~%" (list ',e ,v))
       (force-output *terminal-io*)
       ,v)))

(defclass env ()
  ((ess :initarg :ess :reader ess)
   (fss :initarg :fss :reader fss))
  (:default-initargs :ess 3 :fss 4))

(defun make-env (ess fss)
  (assert (and (integerp ess) (>= ess 0) (integerp fss) (>= fss 0)))
  (make-instance 'env :ess ess :fss fss))

(defmethod print-object ((env env) s)
  (format s "#env[~D ~D]" (ess env) (fss env)))

(defparameter *env* (make-instance 'env))

(defclass num () ())

(defmethod equalu ((n1 num) (n2 num))
  (and (eql (exactp n1) (exactp n2))
       (eql (to-rational n1) (to-rational n2))))

(defclass unum (num)
  ((sign :initarg :sign :reader sign)
   (expo :initarg :expo :reader expo)
   (frac :initarg :frac :reader frac)
   (ubit :initarg :ubit :reader ubit)
   (e-size :initarg :e-size :reader e-size)
   (f-size :initarg :f-size :reader f-size)
   (ess :initarg :ess :reader ess)
   (fss :initarg :fss :reader fss)))

(defun make-unum (&key (sign '+) (expo 0 expo-p) (frac 0 frac-p) (ubit 0)
		  (e-size 1 es-p) (f-size 1 fs-p)
		  (ess (ess *env*)) (fss (fss *env*)))
  (assert
   (and (member sign '(+ -)) (integerp expo) (integerp frac)
	(integerp ubit) (<= 0 ubit 1) (integerp e-size) (> e-size 0)
	(integerp f-size) (> f-size 0)
	(integerp ess) (>= ess 0) (integerp fss) (>= fss 0)))
  (let* ((mes (ash 1 ess))
	 (mfs (ash 1 fss))
	 (e-size
	  (if expo-p
	      (if (> (integer-length (abs expo)) mes)
		  (error "expo too big: ~D" expo)
		  (if es-p
		      (if (> e-size mes)
			  (error "e-size too big: ~D" e-size)
			  (if (> (integer-length (abs expo)) e-size)
			      (error "expo too big: ~D" expo)
			      e-size))
		      (setf e-size (max 1 (integer-length (abs expo))))))
	      (if es-p
		  (if (> e-size mes)
		      (error "e-size too big: ~D" e-size)
		      e-size)
		  e-size)))
	 (f-size
	  (if frac-p
	      (if (> (integer-length (abs frac)) mfs)
		  (error "frac too big: ~D" frac)
		  (if fs-p
		      (if (> f-size mfs)
			  (error "f-size too big: ~D" f-size)
			  (if (> (integer-length (abs frac)) f-size)
			      (error "frac too big: ~D" frac)
			      f-size))
		      (setf f-size (max 1 (integer-length (abs frac))))))
	      (if fs-p
		  (if (> f-size mfs)
		      (error "f-size too big: ~D" f-size)
		      f-size)
		  f-size))))
    (make-instance
     'unum :sign sign :expo (mask-field (byte e-size 0) expo)
     :frac (mask-field (byte f-size 0) frac) :e-size e-size :f-size f-size
     :ubit ubit :ess ess :fss fss)))

(defmethod copy-unum ((u unum)
		      &key (sign (sign u)) (expo (expo u)) (frac (frac u))
		      (ubit (ubit u)) (e-size (e-size u)) (f-size (f-size u))
		      (ess (ess u)) (fss (fss u)))
  (make-unum
   :sign sign :expo expo :frac frac :ubit ubit :e-size e-size :f-size f-size
   :ess ess :fss fss))

(defmethod eqlu ((u1 unum) (u2 unum))
  (or (eq u1 u2)
      (and (eq (sign u1) (sign u2))
	   (eql (expo u1) (expo u2))
	   (eql (frac u1) (frac u2))
	   (eql (ubit u1) (ubit u2))
	   (eql (e-size u1) (e-size u2))
	   (eql (f-size u1) (f-size u2))
	   (eql (ess u1) (ess u2))
	   (eql (fss u1) (fss u2)))))

(defvar *print-unum-real* t)

(defmethod print-object ((u unum) s)
  (format
   s "~<~3I#u~[(~;[~]~B ~V,'0B ~V,'0B ~B~@[~< ~V,'0B~:>~]~@[~< ~V,'0B~:>~]~
        ~@[, ~:_~W~]~[)~;]~]~:>"
   (list
    (if (eql 1 (ubit u)) 0 1)
    (if (eq (sign u) '+) 0 1) (e-size u) (expo u) (f-size u) (frac u)
    (ubit u) (when (> (ess u) 0) (list (ess u) (1- (e-size u))))
    (when (> (fss u) 0) (list (fss u) (1- (f-size u))))
    (when *print-unum-real* (hnum u)) (if (eql 1 (ubit u)) 0 1))))

(defmethod diffu ((u1 unum) (u2 unum))
  (make-unum ; TODO: improve
   :sign (if (eq (sign u1) (sign u2)) '+ '-)
   :expo (logxor (expo u1) (expo u2))
   :frac (logxor (frac u1) (frac u2))
   :ubit (logxor (ubit u1) (ubit u2))
   :e-size (max (e-size u1) (e-size u2))
   :f-size (max (f-size u1) (f-size u2))
   :ess (max (ess u1) (ess u2))
   :fss (max (fss u1) (fss u2))))

;;; sizes
(defmethod utag-size ((u unum))
  (+ 1 (ess u) (fss u)))
(defmethod utag-size ((env env))
  (+ 1 (ess env) (fss env)))

(defmethod max-e-size ((u unum))
  (ash 1 (ess u)))
(defmethod max-e-size ((env env))
  (ash 1 (ess env)))

(defmethod max-f-size ((u unum))
  (ash 1 (fss u)))
(defmethod max-f-size ((env env))
  (ash 1 (fss env)))

(defmethod num-bits ((u unum))
  (+ 1 (e-size u) (f-size u) (utag-size u)))
(defmethod num-bits ((env env))
  (max-bits env))

(defmethod max-bits ((u unum))
  (+ 1 (max-e-size u) (max-f-size u) (utag-size u)))
(defmethod max-bits ((env env))
  (+ 1 (max-e-size env) (max-f-size env) (utag-size env)))

(defun byte-mask (len val)
  (mask-field (byte len 0) val))

;;; derived constants
(defmethod ulp ((u unum) &key (sign (sign u)) (e-size (e-size u))
		(f-size (f-size u)))
  (copy-unum u :sign sign :expo 0 :frac 1 :ubit 0
	     :e-size e-size :f-size f-size))
(defmethod ulp ((env env) &key (sign '+) (e-size (max-e-size env))
		(f-size (max-f-size env)))
  (make-unum :sign sign :frac 1
	     :e-size e-size :f-size f-size
	     :ess (ess env) :fss (fss env)))
(defmethod almost-zero-unum ((u unum) &key (sign (sign u)))
  (make-unum :sign sign :frac 0 :ubit 1
	     :e-size (e-size u) :f-size (f-size u)
	     :ess (ess u) :fss (fss u)))
(defmethod almost-zero-unum ((env env) &key (sign '+))
  (assert (member sign '(+ -)))
  (make-unum :sign sign :frac 0 :ubit 1
	     :e-size (max-e-size env) :f-size (max-f-size env)
	     :ess (ess env) :fss (fss env)))
(defmethod open-zero-unum ((env env) &key (sign '+))
  (assert (member sign '(+ -)))
  (make-unum :sign sign :ubit 1 :e-size 1 :f-size 1
	     :ess (ess env) :fss (fss env)))

(defmethod small-subnormal ((u unum) &key (sign (sign u)))
  (assert (member sign '(+ -)))
  (copy-unum u :sign sign :expo 0 :frac 1 :ubit 0
	     :e-size (max-e-size u) :f-size (max-f-size u)))
(defmethod small-subnormal ((env env) &key (sign '+))
  (assert (member sign '(+ -)))
  (make-unum :sign sign :frac 1
	     :e-size (max-e-size env) :f-size (max-f-size env)
	     :ess (ess env) :fss (fss env)))

(defmethod small-normal ((u unum) &key (sign (sign u)))
  (assert (member sign '(+ -)))
  (copy-unum u :sign sign :expo 1 :frac 0 :ubit 0
	     :e-size (max-e-size u) :f-size (max-f-size u)))
(defmethod small-normal ((env env) &key (sign '+))
  (assert (member sign '(+ -)))
  (make-unum :sign sign :expo 1
	     :e-size (max-e-size env) :f-size (max-f-size env)
	     :ess (ess env) :fss (fss env)))

(defmethod smallest ((u unum) &key (sign (sign u)))
  (if (and (eql (e-size u) (max-e-size u)) (eql (f-size u) (max-f-size u)))
      (small-subnormal u :sign sign)
      (progn
	(assert (member sign '(+ -)))
	(copy-unum u :sign sign :expo 0 :frac 1))))
;; (defmethod smallest ((env env) &key (sign '+))
;;   (small-subnormal env :sign sign))

(defmethod biggest ((u unum) &key (sign (sign u)))
  (if (and (eql (e-size u) (max-e-size u)) (eql (f-size u) (max-f-size u)))
      (max-unum u :sign sign)
      (progn
	(assert (member sign '(+ -)))
	(copy-unum u :sign sign :expo (byte-mask (e-size u) -1)
		   :ubit 0 :frac (byte-mask (f-size u) -1)))))
;; (defmethod biggest ((env env) &key (sign '+))
;;   (max-unum env :sign sign))

(defmethod max-unum ((u unum) &key (sign (sign u)))
  (assert (member sign '(+ -)))
  (copy-unum u :sign sign :expo (byte-mask (max-e-size u) -1)
	     :frac (byte-mask (max-f-size u) -2)
	     :ubit 0 :e-size (max-e-size u) :f-size (max-f-size u)))
(defmethod max-unum ((env env) &key (sign '+))
  (assert (member sign '(+ -)))
  (make-unum :sign sign :expo (byte-mask (max-e-size env) -1)
	     :frac (byte-mask (max-f-size env) -2)
	     :e-size (max-e-size env) :f-size (max-f-size env)
	     :ess (ess env) :fss (fss env)))
(defmethod almost-inf-unum ((u unum) &key (sign (sign u)))
  (assert (member sign '(+ -)))
  (make-unum :sign sign :expo (byte-mask (e-size u) -1)
	     :frac (byte-mask (f-size u) -2) :ubit 1
	     :e-size (e-size u) :f-size (f-size u)
	     :ess (ess u) :fss (fss u)))
(defmethod almost-inf-unum ((env env) &key (sign '+))
  (assert (member sign '(+ -)))
  (make-unum :sign sign :expo (byte-mask (max-e-size env) -1)
	     :frac (byte-mask (max-f-size env) -2) :ubit 1
	     :e-size (max-e-size env) :f-size (max-f-size env)
	     :ess (ess env) :fss (fss env)))
(defun warlpirip (env)
  (and (eql (ess env) 0) (eql (fss env) 0)))
(defmethod open-inf-unum ((env env) &key (sign '+))
  (assert (member sign '(+ -)))
  (if (warlpirip env)
      (make-unum :sign sign :expo 1 :ubit 1 :e-size 1 :f-size 1 :ess 0 :fss 0)
      (make-unum :sign sign :expo 1 :frac 1 :ubit 1 :e-size 1 :f-size 1
		 :ess (ess env) :fss (fss env))))

(defmethod unum-inf ((u unum) &key (sign (sign u)))
  (assert (member sign '(+ -)))
  (copy-unum u :sign sign :expo (byte-mask (max-e-size u) -1)
	     :frac (byte-mask (max-f-size u) -1) :ubit 0
	     :e-size (max-e-size u) :f-size (max-f-size u)))
(defmethod unum-inf ((env env) &key (sign '+))
  (assert (member sign '(+ -)))
  (make-unum :sign sign :expo (byte-mask (max-e-size env) -1)
	     :frac (byte-mask (max-f-size env) -1)
	     :e-size (max-e-size env) :f-size (max-f-size env)
	     :ess (ess env) :fss (fss env)))
(defmethod inf-p ((u unum))
  (eqlu u (unum-inf u)))

(defmethod unum-nan ((u unum) &key (sign (eql (sign u) '-)))
  (assert (member sign '(t nil)))
  (when (eq sign t) (setf sign '-))
  (when (eq sign nil) (setf sign '+))
  (copy-unum u :sign sign :expo (byte-mask (max-e-size u) -1)
	     :frac (byte-mask (max-f-size u) -1) :ubit 1
	     :e-size (max-e-size u) :f-size (max-f-size u)))
(defmethod unum-nan ((env env) &key (sign nil))
  (assert (member sign '(t nil)))
  (when (eq sign t) (setf sign '-))
  (when (eq sign nil) (setf sign '+))
  (make-unum :sign sign :expo (byte-mask (max-e-size env) -1)
	     :frac (byte-mask (max-f-size env) -1) :ubit 1
	     :e-size (max-e-size env) :f-size (max-f-size env)
	     :ess (ess env) :fss (fss env)))
(defmethod nan-p ((u unum))
  (eqlu u (unum-nan u)))

;;; miscellaneous
(defmethod exact ((u unum))
  (if (or (eql (ubit u) 0) (nan-p u))
      u
      (copy-unum u :ubit 0)))

(defmethod inexact ((u unum))
  (if (or (eql (ubit u) 1) (inf-p u))
      u
      (copy-unum u :ubit 1)))

(defmethod exactp ((u unum))
  (eql 0 (ubit u)))

(defmethod inexactp ((u unum))
  (eql 1 (ubit u)))

(defclass bound ()
  ((lo :initarg :lo :reader lo)
   (hi :initarg :hi :reader hi)))

(defmethod hi* ((b bound)) ; singleton case
  (or (hi b) (lo b)))
(defmethod lo ((r real))
  r)
(defmethod hi* ((r real))
  r)
(defmethod lo ((pair cons))
  (car pair))
(defmethod hi* ((pair cons))
  (cdr pair))

(defmethod equalu ((b1 bound) (b2 bound))
  (or (eq b1 b2)
      (and (not (nan-p b1)) (not (nan-p b2))
	   (equalu (lo b1) (lo b2)) (equalu (hi* b1) (hi* b2)))))

(defmethod num< ((n1 num) (n2 num))
  (cond
    ((or (nan-p n1) (nan-p n2)) nil)
    (t (let* ((r1 (to-real n1))
	      (r2 (to-real n2))
	      (r1* (hi* r1))
	      (r2* (lo r2)))
	 (or (< r1* r2*)
	     (and (eql r1* r2*) (and (exactp n1) (not (exactp n2)))))))))

(defmethod num< ((b1 bound) (b2 bound))
  (cond
    ((or (nan-p b1) (nan-p b2)) nil)
    (t (let* ((r1 (to-real (hi* b1)))
	      (r2 (to-real (lo b2)))
	      (r1* (hi* r1))
	      (r2* (lo r2)))
	 (or (< r1* r2*)
	     (and (eql r1* r2*)
		  (not (and (exactp (hi* b1)) (exactp (lo b2))))))))))

(defmethod num> ((n1 num) (n2 num))
  (cond
    ((or (nan-p n1) (nan-p n2)) nil)
    (t (let* ((r1 (to-real n1))
	      (r2 (to-real n2))
	      (r1* (lo r1))
	      (r2* (hi* r2)))
	 (or (> r1* r2*)
	     (and (eql r1* r2*) (and (exactp n2) (not (exactp n1)))))))))

(defmethod num> ((b1 bound) (b2 bound))
  (cond
    ((or (nan-p b1) (nan-p b2)) nil)
    (t (let* ((r1 (to-real (lo b1)))
	      (r2 (to-real (hi* b2)))
	      (r1* (lo r1))
	      (r2* (hi* r2)))
	 (or (> r1* r2*)
	     (and (eql r1* r2*)
		  (not (and (exactp (lo b1)) (exactp (hi* b2))))))))))

(defun lessp (n1 n2 &rest nums)
  (if nums
      (and (num< n1 n2)
	   (not (null (reduce (lambda (x y) (and x (num< x y) y)) nums
			      :initial-value n2))))
      (num< n1 n2)))

(defun greaterp (n1 n2 &rest nums)
  (if nums
      (and (num> n1 n2)
	   (not (null (reduce (lambda (x y) (and x (num> x y) y)) nums
			      :initial-value n2))))
      (num> n1 n2)))

(defmethod negate ((u unum))
  (cond
    ((eqlu u (make-unum)) u)
    (t (copy-unum u :sign (if (eq '+ (sign u)) '- '+)))))

(defmethod inc-e-size ((u unum) &optional (by 1))
  (let* ((e (expo u))
	 (b (expo-bias u))
	 (f (frac u))
	 (es (e-size u))
	 (fs (f-size u))
	 (fl (integer-length f))
	 (sl (- fs fl -1))
	 (r
	  (if (< es (max-e-size u))
	      (cond
		((and (eql 0 e) (eql 0 f))
		 (copy-unum u :e-size (1+ es) :sign '+))
		((> e 0) (copy-unum u :expo (+ e b 1) :e-size (1+ es)))
		((< (+ 2 b) sl) (copy-unum
				 u :frac (min (ash f (+ 2 b)) (1- (ash 1 fs)))
				 :e-size (1+ es)))
		(t (copy-unum u :expo (- b -2 sl -1) :e-size (1+ es)
			      :frac (ash (- f (ash 1 (1- fl))) sl))))
	      u))) ; TODO: make loud
    (if (> by 1)
	(inc-e-size r (1- by))
	r)))

(defmethod dec-e-size ((u unum) &optional (by 1))
  (let* ((e (expo u))
	 (f (frac u))
	 (es (e-size u))
	 (fs (f-size u))
	 (mfs (max-f-size u))
	 (sl (ash 1 (- es 2)))
	 (l2 (ash (logand e (* 3 sl)) (- 2 es)))
	 (e2 (+ (ash (logand e (* 2 sl)) -1) (logand e (1- sl))))
	 (r
	  (cond
	    ((or (eql es 1) (inf-p u) (nan-p u)) u)
	    ((eql e 0)
	     (multiple-value-bind (f2 r2) (floor f (ash 1 sl))
	       (copy-unum u :frac f2 :ubit (signum (+ (ubit u) r2))
			  :e-size (1- es))))
	    ((eql 0 l2)
	     (multiple-value-bind (f2 r2)
		 (floor (* (+ (ash 1 fs) f) (ash 1 e))
			(ash 1 (1+ (ash 1 (- es 2)))))
	       (copy-unum u :expo 0 :frac f2 :ubit (signum (+ (ubit u) r2))
			  :e-size (1- es))))
	    ((<= l2 2)
	     (if (eql 0 e2)
		 (multiple-value-bind (f2 r2) (floor (+ (ash 1 fs) f) 2)
		   (copy-unum u :expo 0 :frac f2 :ubit (signum (+ (ubit u) r2))
			      :e-size (1- es)))
		 (copy-unum u :expo e2 :e-size (1- es))))
	    (t (copy-unum u :expo -1 :frac -1 :ubit 1 :e-size (1- es))))))
    (if (> by 1)
	(dec-e-size r (1- by))
	r)))

(defmethod inc-f-size ((u unum) &optional (by 1))
  (let ((r (if (< (f-size u) (max-f-size u))
	       (copy-unum u :frac (* 2 (frac u)) :f-size (1+ (f-size u)))
	       u))) ; TODO: make loud
    (if (> by 1)
	(inc-f-size r (1- by))
	r)))

(defmethod dec-f-size ((u unum) &optional (by 1))
  (let* ((fs (f-size u))
	 (r (cond
	      ((or (eql 1 fs) (inf-p u) (nan-p u)) u)
	      (t (multiple-value-bind (f2 r2) (floor (frac u) 2)
		   (copy-unum u :frac f2 :ubit (signum (+ (ubit u) r2))
			      :f-size (1- fs)))))))
    (if (> by 1)
	(dec-f-size r (1- by))
	r)))

(defmethod inc-ubit ((u unum))
  (assert (eql 0 (ubit u)))
  (inexact u))

(defmethod dec-ubit ((u unum))
  (assert (eql 0 (ubit u)))
  (let ((f (1- (frac u))))
    (if (< f 0) ; lost
	(let ((e (expo u)))
	  (assert (> e 0))
	  (copy-unum u :expo (1- e) :frac f :ubit 1))
	(copy-unum u :frac f :ubit 1))))

(defmethod promote ((u1 unum) (u2 unum))
  (if (or (nan-p u1) (nan-p u2))
      (unum-nan *env*)
      (progn
	(assert (and (eql 0 (ubit u1)) (eql 0 (ubit u2))
		     (eql (ess u1) (ess u2)) (eql (fss u1) (fss u2))))
	(let ((e1 (e-size u1))
	      (f1 (f-size u1))
	      (e2 (e-size u2))
	      (f2 (f-size u2)))
	  (loop until (and (eql e1 e2) (eql f1 f2))
	     when (< e1 e2) do (setf u1 (inc-e-size u1) e1 (1+ e1))
	     when (< e2 e1) do (setf u2 (inc-e-size u2) e2 (1+ e2))
	     when (< f1 f2) do (setf u1 (inc-f-size u1) f1 (1+ f1))
	     when (< f2 f1) do (setf u2 (inc-f-size u2) f2 (1+ f2))
	     finally (return (values u1 u2)))))))

(defun to-signum (x)
  (case x
    ((+ t) 1)
    ((nil) 0)
    ((-) -1)
    (t (signum x))))

(defmethod expo-bias ((u unum))
  (+ (ash 1 (1- (e-size u))) (to-signum (expo u)) -2))

(defmethod expo-value ((u unum))
  (- (expo u) (expo-bias u)))

(defmethod frac-value ((u unum))
  (/ (+ (* (to-signum (expo u)) (ash 1 (f-size u))) (frac u))
     (ash 1 (f-size u))))

(defun lisp-inf (proto)
  (let ((pos (= proto (abs proto))))
    #+sbcl
    (cond
      ((or (sb-int::single-float-p proto) (integerp proto))
       (if pos
	   sb-ext:single-float-positive-infinity
	   sb-ext:single-float-negative-infinity))
      (t (if pos
	     sb-ext:double-float-positive-infinity
	     sb-ext:double-float-negative-infinity)))
    #-sbcl
    (if pos
	'positive-infinity
	'negative-infinity)))

(defun lisp-biggest (proto)
  (let ((pos (= proto (abs proto))))
    (cond
      ((or (sb-int::single-float-p proto) (integerp proto))
       (if pos
	   most-positive-single-float
	   most-negative-single-float))
      (t (if pos
	     most-positive-double-float
	     most-negative-double-float)))))

(defmethod to-rational ((u unum) &optional (proto 1.0d0))
  ;;(assert (eql 0 (ubit u)))
  (let ((proto (* (to-signum (sign u)) proto)))
    (if (eqlu u (unum-inf u))
	(lisp-inf proto)
	(* (to-signum (sign u)) (expt 2 (expo-value u)) (frac-value u)))))

(defmethod to-real ((u unum) &optional (proto 1.0d0))
  (cond
    ((nan-p u) (if (eql (sign u) '+) 'qnan 'snan))
    ((eql (ubit u) 0) (to-rational u proto))
    ((eqlu (exact u) (biggest u))
     (if (eq (sign u) '+)
	 (cons (to-rational (biggest u) proto) (lisp-inf (abs proto)))
	 (cons (lisp-inf (- (abs proto))) (to-rational (biggest u) proto))))
    (t (multiple-value-bind (u u+ulp) (widen-to-ulp* u)
	 (cons (to-rational u proto) (to-rational u+ulp proto))))))

(defmethod min-rational ((u unum) &key (sign (sign u)))
  (to-rational (small-subnormal u :sign sign)))
(defmethod min-rational ((env env) &key (sign '+))
  (to-rational (small-subnormal env :sign sign)))

(defmethod max-rational ((u unum) &key (sign (sign u)))
  (to-rational (max-unum u :sign sign)))
(defmethod max-rational ((env env) &key (sign '+))
  (to-rational (max-unum env :sign sign)))

(defmethod scale ((f real))
  (assert (not (eql f (lisp-inf f)))) ; TODO: lisp-nan (?)
  (if (zerop f)
      0 ; precision issues...
      (floor (log (abs (or (ignore-errors (float f 1.0d0)) f)) 2))))

(defmethod min-e-size ((f real))
  (assert (not (eql f (lisp-inf f)))) ; TODO: lisp-nan (?)
  (let ((sf (scale f)))
    (if (or (zerop f) (eql 1 sf))
	1
	(1+ (ceiling (log (1+ (abs (1- sf))) 2))))))

(defmethod unum ((f real) &key (env *env*) (proto (if (floatp f) f 1.0d0)))
  (declare (ignorable proto))
  (let* ((*env* env)
	 (af (abs f))
	 (sign (if (= f af) '+ '-)))
    (cond
      ((eql f (lisp-inf f)) (unum-inf env :sign sign)) ; TODO: lisp-nan (?)
      ((zerop f) (make-unum))
      ((> af (max-rational env)) (almost-inf-unum env :sign sign))
      ((< af (min-rational env)) (almost-zero-unum env :sign sign))
      ((< af (to-rational (small-normal env)))
       (let* ((ssn (small-subnormal env))
	      (r (/ af (to-rational ssn)))
	      (u (copy-unum ssn :sign sign
			    :frac (floor r) :ubit (if (= r (floor r)) 0 1))))
	 (loop while (and (eql 0 (ubit u)) (evenp (frac u)))
	    do (multiple-value-bind (q) (floor (frac u) 2) ; ubit stays 0
		 (setf u (copy-unum u :frac q :f-size (1- (f-size u)))))
	    finally (return u))))
      (t (let* ((sf (scale f))
		(mes (min-e-size f))
		(fs 0)
		(r (/ af (expt 2 sf)))
		(mfs (max-f-size env)))
	   (loop while (and (not (= r (floor r))) (< fs mfs))
	      do (setf fs (1+ fs)
		       r (* 2 r)))
	   (if (= r (floor r))
	       (let ((es (if (< af 2)
			     (ignore-errors (log (- 1 (log af 2)) 2))
			     -1)))
		 (if (and es (= es (floor es)) (<= 0 es))
		     (ulp env :sign sign :e-size (1+ (floor es)) :f-size 1)
		     (make-unum
		      :sign sign :expo (+ sf (ash 1 (1- mes)) -1)
		      :frac (floor (- r (expt 2 (scale r))))
		      :e-size mes :f-size (max 1 fs))))
	       (let* ((fs (expt 2 (- sf mfs)))
		      (f2 (* (ceiling (/ af fs)) fs))
		      (sf2 (scale f2))
		      (mes2 (max mes (min-e-size f2)))
		      (g (+ sf2 (ash 1 (1- mes2)) -1))
		      (h (floor (* (1- (/ f2 (expt 2 sf2))) (ash 1 mfs))))
		      (mes3 (max-e-size env))) ; NOTE: small env?
		 (dec-ubit
		  (make-unum
		   :sign sign :expo (min (1- (ash 1 mes3)) g)
		   :frac (min (1- (ash 1 mfs)) h)
		   :e-size (min mes3 mes2) :f-size mfs)))))))))

(defclass ubound (bound) ())

(defmethod nan-p ((ub ubound))
  (or (nan-p (lo ub)) (nan-p (hi* ub))))

(defmethod inf-p ((ub ubound))
  (and (inf-p (lo ub)) (null (hi ub))))

(defun make-ubound (ul &optional ur)
  (assert ul)
  (if ur
      (progn
	(assert (or (nan-p ul) (nan-p ur) (not (greaterp ul ur))))
	(assert (implies (let ((z (make-unum)))
			   (and (equalu z ul) (equalu z ur)))
			 (implies (eq '+ (sign ul)) (eq '+ (sign ur)))))
	(if (eqlu ul ur)
	    (make-instance 'ubound :lo ul :hi nil)
	    (make-instance 'ubound :lo ul :hi ur)))
      (make-instance 'ubound :lo ul :hi nil)))

(defmethod ubound ((u unum))
  (make-ubound u))

(defmethod ubound ((r real))
  (make-ubound (unum r)))

(defmethod copy-ubound ((ub ubound) &key (lo (lo ub)) (hi (hi ub)))
  (make-ubound lo hi))

(defmethod eqlu ((ub1 ubound) (ub2 ubound))
  (or (eq ub1 ub2)
      (and (eqlu (lo ub1) (lo ub2))
	   (if (null (hi ub1))
	       (null (hi ub2))
	       (eqlu (hi ub1) (hi ub2))))))

(defvar *ubits-moved* nil)
(defvar *unums-moved* nil)
(defvar *ubounds-moved* nil)

(defmacro without-scratchpad (opts &body body)
  (declare (ignorable opts))
  `(let ((*ubits-moved* nil)
	 (*unums-moved* nil)
	 (*ubounds-moved* nil))
     ,@body))

(defclass uarray ()
  ((arr :initarg :arr :accessor arr)))

(defmethod eqlu ((ua1 uarray) (ua2 uarray))
  (or (eq ua1 ua2)
      (and (eql (length (arr ua1)) (length (arr ua2)))
	   (every #'eqlu (arr ua1) (arr ua2)))))

(defmethod uref ((a uarray) &rest indices)
  (apply #'aref (arr a) indices))

(defsetf uref (a &rest indices) (v)
  `(setf (apply #'aref (arr ,a) (list ,@indices)) ,v))

(defclass barray (uarray) ())

(defclass ubarray (barray) ())

(defclass ubox (uarray) ())

(defun make-ubarray (arr)
  (assert (eq (type-of (aref arr 0)) 'ubound))
  (make-instance 'ubarray :arr arr))

(defmethod print-object ((ubv ubarray) s)
  (format s "#ubarray[ ~A ]" (arr ubv)))

(defmethod disjointp ((b1 bound) (b2 bound))
  (or (lessp b1 b2) (greaterp b1 b2)))

(defmethod jointp ((b1 t) (b2 t))
  (unless (or (nan-p b1) (nan-p b2))
    (not (disjointp b1 b2))))

(defmethod negate ((ub ubound))
  (cond
    ((null (hi ub)) (make-ubound (negate (lo ub))))
    (t (make-ubound (negate (hi ub)) (negate (lo ub))))))

(defmethod print-object ((ub ubound) s)
  (let* ((lu (lo ub))
	 (ru (hi ub))
	 (lp (if (eql 1 (ubit lu)) 0 1))
	 (rp (if ru (if (eql 1 (ubit ru)) 0 1) lp))
	 (*print-unum-real* (not ru)))
    (format
     s "~<~4I#ub~[(~;[~]~W~@[, ~:_~W~]~
        ~@[; ~:_~<~1I~[(~;[~]~W, ~:_~W~[)~;]~]~:>~]~[)~;]~]~:>"
     (list
      lp
      lu ru
      (when ru
	(list lp (hnum lu) (hnum ru) rp))
      rp))))

(defmethod num-bits ((ub ubound))
  (+ 1 (num-bits (lo ub)) (if (hi ub) (num-bits (hi ub)) 0)))

(defmethod num-unums ((ub ubound))
  (if (hi ub) 2 1))

(defmethod unify+ ((ub ubound))
  (let ((l (lo ub))
	(r (hi* ub)))
    (cond
      ((equalu l r)
       (ubound (copy-unum (unum (to-rational l)) :ubit (ubit l)))); was gbound..
      ((or (jointp ub (ubound 0)) (jointp ub (ubound 1))
	   (jointp ub (ubound 2)) (jointp ub (ubound 3)))
       ub)
      (t (let* ((z (make-unum :e-size (max-e-size *env*)
			      :f-size (max-f-size *env*)))
		(lp (promote (exact (lo (widen-to-ulp l))) z))
		(l (if (inexactp l)
		       (or (ignore-errors (inc-ubit lp)) (unum-nan *env*))
		       (if (or (equalu lp z) (nan-p lp))
			   lp
			   (dec-ubit lp))))
		(rp (promote (exact (hi* (widen-to-ulp r))) z))
		(r (if (inexactp r)
		       (if (or (equalu rp z) (nan-p rp))
			   rp
			   (dec-ubit rp))
		       (or (ignore-errors (inc-ubit rp)) (unum-nan *env*)))))
	   (cond
	     ((or (nan-p l) (nan-p r)) (make-instance 'gbound :nan-p t))
	     ((eqlu l r) (ubound l))
	     (t (let ((rgl (lo (gbound r)))
		      (g-inf (gnum (unum-inf *env*))))
		  (if (and (open-p rgl) (inf-p rgl))
		      (if (lessp (max-unum *env*) l)
			  (almost-inf-unum *env*)
			  (loop while (lessp (hi (gbound l)) g-inf)
			     if (> (e-size l) 1) do (setf l (dec-e-size l))
			     else do (setf l (dec-f-size l))
			     finally (return (ubound l))))
		      (progn
			(loop
			   while
			     (and
			      (not (eqlu l r))
			      (let ((lg (gbound (dec-e-size l)))
				    (rg (gbound (dec-e-size r))))
				(lessp (lo lg) (lo rg))
				(lessp (hi lg) (hi rg) g-inf))
			      (> (e-size l) 1))
			   do (setf l (dec-e-size l)
				    r (dec-e-size r)))
			(loop
			   while
			     (and
			      (not (eqlu l r))
			      (not (eql (frac l) (frac r))) (> (f-size l) 1))
			   do (setf l (dec-f-size l)
				    r (dec-f-size r)))
			(if (and
			     (not (eqlu l r))
			     (eql 0 (expo l)) (eql 0 (frac l)) (eql 1 (ubit l))
			     (lessp r (unum 1)))
			    (let ((n
				   (min
				    (max-e-size *env*)
				    (floor
				     (log
				      (-
				       1
				       (log
					(to-rational
					 (hi (gbound (copy-unum r :ubit 1))))
					2))
				      2)))))
			      (ubound
			       (dec-ubit (unum (expt 2 (- 1 (ash 1 n)))))))
			    (if (eqlu l r) (ubound l) ub))))))))))))

(defmethod unify ((ub ubound))
  (let ((lu (lo ub))
	(ru (hi* ub)))
    (cond
      ((or (nan-p lu) (nan-p ru)) ub)
      ((eq (sign lu) '+) (unify+ ub))
      (t (negate (unify+ (negate ub)))))))

(defun widen-to-ulp* (u &key (ubit 0) (min nil))
  (let* ((fs (f-size u))
	 (mfs (if min (max-f-size u) fs))
	 (fsd (- mfs fs)))
    (multiple-value-bind (e2 f2)
	(floor (1+ (ash (frac u) fsd)) (ash 1 mfs))
      (let* ((es (e-size u))
	     (es-small (>= (+ (expo u) e2) (ash 1 es)))
	     (mes (max-e-size u))
	     (ue (inc-e-size u))
	     (u* (copy-unum (if es-small ue u) :ubit ubit))
	     (u+ulp (if (and es-small (eql es mes)) ; could not increment e-size
			(copy-unum u :frac (if (eql e2 0) f2 (frac u))
				   :f-size mfs :ubit ubit)
			(copy-unum (if es-small ue u)
				   :expo (+ (expo u) e2)
				   :frac (if (eql 0 e2) f2 (ash f2 -1))
				   :f-size mfs
				   :ubit ubit))))
	(if (eq (sign u) '+)
	    (values u* u+ulp)
	    (values u+ulp u*))))))

(defmethod widen-to-ulp ((u unum))
  (cond
    ((nan-p u) (make-ubound u u))
    ((eql 0 (ubit u)) (make-ubound u u))
    ((eqlu (exact u) (biggest u))
     (if (eq (sign u) '+)
	 (make-ubound (biggest u) (almost-inf-unum u))
	 (make-ubound (almost-inf-unum u) (biggest u))))
    (t (multiple-value-bind (u u+ulp) (widen-to-ulp* u :ubit 1)
	 (make-ubound u u+ulp)))))

(defmethod widen-to-ulp ((ub ubound))
  (let ((lu (lo ub))
	(ru (hi* ub)))
    (cond
      ((or (nan-p lu) (nan-p ru)) ub)
      (t (let ((lub (widen-to-ulp lu))
	       (rub (widen-to-ulp ru)))
	   (make-ubound (lo lub) (hi* rub)))))))

(defmethod compress ((ub ubound) ratio)
  (let* ((ub* (widen-to-ulp ub))
	 (wb (- (to-rational (hi* ub*)) (to-rational (lo ub*))))
	 (uub (unify ub))
	 (uub* (widen-to-ulp uub))
	 (uwb (- (to-rational (hi* uub*)) (to-rational (lo uub*)))))
    (cond
      ((eql uwb (lisp-inf uwb)) (if (>= ratio 1) uub ub))
      ((zerop uwb) uub)
      ((>= (* (/ wb uwb) (/ (num-bits ub) (num-bits uub))) ratio) uub)
      (t ub))))

(defclass unpacked-unum (unum)
  ((neg-p :initarg :neg-p :reader neg-p)
   (nan-p :initarg :nan-p :reader nan-p)
   (inf-p :initarg :inf-p :reader inf-p)
   (zero-p :initarg :zero-p :reader zero-p)
   (pair-p :initarg :pair-p :reader pair-p)
   (unhid :initarg :unhid :reader unhid)))

(defmethod unpack ((u unum) &key (env *env* env-p) pair-p)
  (assert (and (>= (ess env) (ess u)) (>= (fss env) (fss u))))
  (let ((all-max (and (eql (expo u) (byte-mask (e-size u) -1))
		      (eql (frac u) (byte-mask (f-size u) -1))
		      (eql (e-size u) (max-e-size u))
		      (eql (f-size u) (max-f-size u))
		      (eql (ess u) (ess env)) (eql (fss u) (fss env))))
	(zero (and (eql 0 (expo u)) (eql 0 (frac u)))))
    (make-instance
     'unpacked-unum
     :neg-p (and (not (and zero (eql 0 (ubit u)))) (eq (sign u) '-))
     :nan-p (and (eql (ubit u) 1) all-max)
     :inf-p (and (eql (ubit u) 0) all-max)
     :zero-p zero
     :pair-p pair-p
     :sign (sign u)
     :expo (expo u)
     :unhid (if (not (eql 0 (expo u))) 1 0)
     :frac (frac u)
     :ubit (ubit u)
     :e-size (e-size u)
     :f-size (f-size u)
     :ess (if env-p (ess env) (ess u))
     :fss (if env-p (fss env) (fss u)))))

(defmethod pack ((uu unpacked-unum) &key (env *env*))
  (assert (and (>= (max-e-size env) (e-size uu))
	       (>= (max-f-size env) (f-size uu))))
  (copy-unum uu :ess (ess env) :fss (fss env)))

(defmethod print-object ((uu unpacked-unum) s)
  (let ((size (+ 5 (max-e-size uu) 1 (max-f-size uu) 1 (ess uu) (fss uu))))
    (format
     s "~<~VI#u~D~[(~;[~]~B~B~B~B~B ~V,,,'.<~V,'0B~> ~B ~V,,,'.<~V,'0B~> ~B~
        ~@[ ~<~V,'0B~:>~]~@[ ~<~V,'0B~:>~], ~:_~W~[)~;]~]~:>"
     (list
      (+ 3 (ceiling (log (1+ size) 10))) size (if (eql 1 (ubit uu)) 0 1)
      (if (neg-p uu) 1 0) (if (nan-p uu) 1 0) (if (inf-p uu) 1 0)
      (if (zero-p uu) 1 0) (if (pair-p uu) 1 0)
      (max-e-size uu) (e-size uu) (expo uu) (unhid uu)
      (max-f-size uu) (f-size uu) (frac uu) (ubit uu)
      (when (> (ess uu) 0) (list (ess uu) (1- (e-size uu))))
      (when (> (fss uu) 0) (list (fss uu) (1- (f-size uu))))
      (hnum uu) (if (eql 1 (ubit uu)) 0 1)))))

(defclass gnum (num)
  ((nan-p :initarg :nan-p :reader nan-p)
   (a-expo :initarg :a-expo :reader a-expo)
   (e-sign :initarg :e-sign :reader e-sign)
   (a-frac :initarg :a-frac :reader a-frac)
   (f-sign :initarg :f-sign :reader f-sign)
   (open-p :initarg :open-p :reader open-p)
   (inf-p :initarg :inf-p :reader inf-p)))

(defun make-gnum (&key (nan-p nil) (a-expo 0) (e-sign '+) (a-frac 0) (f-sign '+)
		  (open-p nil) (inf-p nil))
  (make-instance
   'gnum :nan-p nan-p :a-expo a-expo :e-sign e-sign :a-frac a-frac
   :f-sign f-sign :open-p open-p :inf-p inf-p))

(defmethod copy-gnum ((g gnum)
		      &key (nan-p (nan-p g))
		      (a-expo (if (slot-boundp g 'a-expo) (a-expo g) 0))
		      (e-sign (if (slot-boundp g 'e-sign) (e-sign g) '+))
		      (a-frac (if (slot-boundp g 'a-frac) (a-frac g) 0))
		      (f-sign (f-sign g)) (open-p (open-p g)) (inf-p (inf-p g)))
  (make-gnum :nan-p nan-p :a-expo a-expo :e-sign e-sign :a-frac a-frac
	     :f-sign f-sign :open-p open-p :inf-p inf-p))

(defmethod print-object ((g gnum) s) ; TODO: proto
  (if (nan-p g)
      (if (open-p g)
	  (format s "#g(nan)")
	  (format s "#g[nan]"))
      (format s "#g~[(~;[~]~D~[)~;]~]"
	      (if (open-p g) 0 1)
	      (hnum g)
	      (if (open-p g) 0 1))))

(defmethod gnum ((u unum) &key (open-p (eql 1 (ubit u)))) ; NOTE: open-p default
  (cond
    ((nan-p u) (make-instance 'gnum :nan-p t :open-p t))
    ((inf-p u)
     (make-instance 'gnum :nan-p nil :f-sign (sign u) :open-p open-p :inf-p t))
    (t (let ((expo (expo-value u))
	     (frac (frac-value u)))
	 (make-instance
	  'gnum
	  :nan-p nil
	  :a-expo (abs expo)
	  :e-sign (if (< expo 0) '- '+)
	  :a-frac frac
	  :f-sign (sign u)
	  :open-p open-p
	  :inf-p nil)))))

(defmethod eqlu ((g1 gnum) (g2 gnum))
  (or (eq g1 g2)
      (and (nan-p g1) (nan-p g2))
      (and (eq (nan-p g1) (nan-p g2))
	   (eq (open-p g1) (open-p g2))
	   (eq (inf-p g1) (inf-p g2))
	   (eq (f-sign g1) (f-sign g2))
	   (implies (not (or (inf-p g1) (inf-p g2)))
		    (and (eq (e-sign g1) (e-sign g2))
			 (eql (a-frac g1) (a-frac g2))
			 (eql (a-expo g1) (a-expo g2)))))))

(defmethod sign ((g gnum))
  (f-sign g))

(defmethod exact ((g gnum))
  (if (exactp g) g (copy-gnum g :open-p nil)))

(defmethod inexact ((g gnum))
  (if (inexactp g) g (copy-gnum g :open-p t)))

(defmethod exactp ((g gnum))
  (not (open-p g)))

(defmethod inexactp ((g gnum))
  (open-p g))

(defmethod gnum ((f real) &key (open-p nil op-p))
  (cond
    ((eql f (lisp-inf f))
     (make-gnum :inf-p t :f-sign (if (eql f (abs f)) '+ '-) :open-p open-p))
    (t (let* ((fr (rationalp f))
	      (r (if fr f (rational f)))
	      (o (if (or fr op-p)
		     open-p
		     (not (eql r (rationalize f))))))
       (make-gnum :f-sign (if (>= r 0) '+ '-) :a-frac (abs r) :open-p o)))))

(defmethod unum ((g gnum) &key (env *env*))
  (cond
    ((nan-p g) (unum-nan env))
    ((inf-p g) (if (open-p g)
		   (open-inf-unum env :sign (f-sign g)) ; FIXME
		   (unum-inf env :sign (f-sign g))))
    (t (let* ((r (to-rational g))
	      (u (unum r :env env))
	      (ubit (signum (+ (ubit u) (to-signum (open-p g))))))
	 (if (or (eql (ubit u) ubit) (warlpirip env))
	     u
	     (copy-unum u :ubit ubit))))))

(defmethod to-rational ((g gnum) &optional (proto 1.0d0))
  (let ((s (to-signum (f-sign g))))
    (if (inf-p g)
	(lisp-inf (* s proto))
	(* s (expt 2 (* (to-signum (e-sign g)) (a-expo g))) (a-frac g)))))

(defmethod to-real ((g gnum) &optional (proto 1.0d0))
  (if (nan-p g)
      'nan
      (to-rational g proto))) ; TODO: inexact (?!)

(defclass gbound (bound)
  ((nan-p :initarg :nan-p :reader nan-p)))

(defun gbound-nan ()
  (make-instance 'gbound :nan-p t))

(defun make-gbound (gl gr)
  (let ((nan-p (or (nan-p gl) (nan-p gr))))
    (assert (or nan-p (not (greaterp gl gr))))
    (make-instance
     'gbound
     :nan-p nan-p
     :lo gl
     :hi gr)))

(defmethod eqlu ((gb1 gbound) (gb2 gbound))
  (or (eq gb1 gb2)
      (and (not (nan-p gb1)) (not (nan-p gb2))
	   (eqlu (lo gb1) (lo gb2)) (eqlu (hi gb1) (hi gb2)))))

(defclass gbarray (barray) ())

(defun make-gbarray (arr)
  (assert (eq (type-of (aref arr 0)) 'gbound))
  (make-instance 'gbarray :arr arr))

(defmethod print-object ((gbv gbarray) s)
  (format s "#gbarray[ ~A ]" (arr gbv)))

(defmethod print-object ((gb gbound) s)
  (if (nan-p gb)
      (format s "#gb(nan)")
      (format s "~<~4I#gb~[(~;[~]~W, ~:_~W~[)~;]~]~:>"
	      (list
	       (if (open-p (lo gb)) 0 1) (lo gb)
	       (hi gb) (if (open-p (hi gb)) 0 1)))))

(defmethod gbound ((u unum))
  (cond
    ((nan-p u) (make-instance 'gbound :nan-p t))
    ((eql 0 (ubit u)) (let ((g (gnum u))) (make-gbound g g)))
    ((eqlu (exact u) (biggest u))
     (if (eq (sign u) '+)
	 (make-gbound
	  (gnum (biggest u) :open-p (inexactp u))
	  (make-instance 'gnum :nan-p nil :inf-p t :f-sign '+ :open-p t))
	 (make-gbound
	  (make-instance 'gnum :nan-p nil :inf-p t :f-sign '- :open-p t)
	  (gnum (biggest u) :open-p (inexactp u)))))
    (t (multiple-value-bind (u u+ulp) (widen-to-ulp* u :ubit 1)
	 (make-gbound (gnum u) (gnum u+ulp))))))

(defmethod gbound ((ub ubound))
  (when *ubits-moved* (incf *ubits-moved* (num-bits ub)))
  (when *unums-moved* (incf *unums-moved* (num-unums ub)))
  (when *ubounds-moved* (incf *ubounds-moved*))
  (let ((lu (lo ub))
	(ru (hi* ub)))
    (cond
      ((or (nan-p lu) (nan-p ru)) (make-instance 'gbound :nan-p t))
      (t (let ((lg (gbound lu))
	       (rg (gbound ru)))
	   (make-gbound (lo lg) (hi rg)))))))

(defmethod ubound-left ((g gnum))
  (cond
    ((and (inf-p g) (eq (f-sign g) '-))
     (if (open-p g)
	 (open-inf-unum *env* :sign '-)
	 (unum-inf *env* :sign '-)))
    (t (let* ((gr (to-rational g))
	      (u (unum g))
	      (ur (to-rational u))
	      (du (if (eql ur 0) u (dec-ubit (exact u)))))
	 (if (eql ur gr)
	     (if (and (open-p g) (eq (f-sign g) '-) (not (eql 0 (a-frac g))))
		 du
		 u)
	     (if (< gr (max-rational *env* :sign '-))
		 (almost-inf-unum *env* :sign '-)
		 (copy-unum u :ubit 1 #+nil(to-signum (open-p g)))))))))

(defmethod ubound-right ((g gnum))
  (cond
    ((and (inf-p g) (eq (f-sign g) '+))
     (if (open-p g)
	 (open-inf-unum *env* :sign '+)
	 (unum-inf *env* :sign '+)))
    ((and (eql 0 (a-frac g)) (open-p g))
     (open-zero-unum *env* :sign '-))
    (t (let* ((gr (to-rational g))
	      (u (unum g))
	      (ur (to-rational u))
	      (du (if (eql ur 0) u (dec-ubit (exact u)))))
	 (if (eql ur gr)
	     (if (and (open-p g) (eq (f-sign g) '+))
		 du
		 u)
	     (if (> gr (max-rational *env*))
		 (almost-inf-unum *env*)
		 (copy-unum u :ubit 1 #+nil(to-signum (open-p g)))))))))

(defmethod ubound ((gb gbound))
  (let ((ub
	 (if (nan-p gb)
	     (ubound (unum-nan *env*))
	     (let* ((lg (lo gb))
		    (rg (hi gb))
		    (lu (unum lg))
		    (ru (unum rg)))
	       (cond
		 ((or (greaterp lg rg)
		      (and (or (open-p lg) (open-p rg)) (eqlu lg rg)))
		  (ubound (unum-nan *env*)))
		 ((eqlu lu ru) (ubound lu))
		 (t (let* ((lu (ubound-left lg))
			   (ru (ubound-right rg))
			   (ub (make-ubound lu ru))
			   (ubu (unify ub)))
		      (cond
			((nan-p ubu) ub)
			(t (if (and (eql (lo (to-real (lo ubu)))
					 (lo (to-real (lo ub))))
				    (eql (hi* (to-real (hi* ubu)))
					 (hi* (to-real (hi* ub)))))
			       ubu
			       ub))))))))))
    (when *ubits-moved* (incf *ubits-moved* (num-bits ub)))
    (when *unums-moved* (incf *unums-moved* (num-unums ub)))
    (when *ubounds-moved* (incf *ubounds-moved*))
    ub))

(defparameter *relative-width* 0.005)

(defmethod relative-width ((ub ubound))
  (let* ((ub* (widen-to-ulp ub))
	 (lu (lo ub*))
	 (ru (hi* ub*)))
    (cond
      ((nan-p lu) 'inf)
      ((or (inf-p lu) (inf-p ru)) 1)
      ((= 0 (to-rational lu) (to-rational ru)) 0)
      (t (let ((lr (to-rational lu))
	       (rr (to-rational ru)))
	   (/ (abs (- rr lr)) (+ (abs lr) (abs rr))))))))

(defmethod relative-width ((u unum))
  (relative-width (ubound u)))

(defmethod expo-small ((ub ubound))
  (let* ((ub* (widen-to-ulp ub))
	 (lu (lo ub*))
	 (ru (hi* ub*)))
    (or (and (eql (ubit (lo ub)) 1)
	     (or (eql (to-rational ru) (max-rational ru :sign '-))
		 (eql (to-rational lu) (min-rational lu :sign '-))))
	(and (eql (ubit (hi* ub)) 1)
	     (or (eql (to-rational lu) (max-rational *env*))
		 (eql (to-rational ru) (min-rational *env*)))))))

(defmethod expo-small ((u unum))
  (expo-small (ubound u)))

(defmethod frac-small ((ub ubound))
  (unless (nan-p ub)
    (> (relative-width ub) *relative-width*)))

(defmethod frac-small ((u unum))
  (unless (nan-p u)
    (frac-small (ubound u))))

(defmacro calculate (expr &key (rel-wid '*relative-width*) (env '*env*))
  (let ((res (gensym))
	(done (gensym))
	(again (gensym)))
    `(let ((*env* ,env)
	   (*relative-width* ,rel-wid))
       (declare (special *env* *relative-width*))
       (block
	   ,done
	 (tagbody
	  ,again
	    (let ((,res ,expr))
	      (when (expo-small ,res)
		(setf *env* (make-env (1+ (ess *env*)) (fss *env*)))
		(go ,again))
	      (when (frac-small ,res)
		(setf *env* (make-env (ess *env*) (1+ (fss *env*))))
		(go ,again))
	      (return-from ,done ,res)))))))

(defclass hnum () ; TODO: num (?)
  ((u :initarg :u :reader u)
   (g :initarg :g :reader g)))

(defmethod print-object ((h hnum) s)
  (labels ((%auto (x &optional neg)
	     (cond ; watch the case order
	       ((consp x) (cons (%auto (car x) neg) (%auto (cdr x) neg)))
	       ((member x '(snan qnan)) (string-downcase (symbol-name x)))
	       ((or (< x 0) neg) (concatenate 'string "-" (%auto (- x))))
	       ((eql x (lisp-inf x)) "inf")
	       ((eql x 0) "0")
	       (t (let ((y (log (denominator x) 2)))
		    (if (= y (floor y))
			(if (= y 0)
			    (format nil "~D" x)
			    (let* ((y (- x (floor x)))
				   (z (floor (log (denominator y) 2))))
			      (format nil "~D.~V,'0D"
				      (floor x) z (* y (expt 10 z)))))
			x #+nil(%auto (rational (float x 1.0d0)))))))))
    (format s "~A" (%auto (to-real h) (eq (sign h) '-)))))

(defmethod hnum ((u unum))
  (make-instance 'hnum :u u :g nil))

(defmethod hnum ((g gnum))
  (make-instance 'hnum :g g :u nil))

(defmethod to-real ((h hnum) &optional (proto 1.0d0))
  (cond
    ((u h) (to-real (u h) proto))
    ((g h) (to-real (g h) proto))
    (t (error "undefined"))))

(defmethod sign ((h hnum))
  (cond
    ((u h) (sign (u h)))
    ((g h) (sign (g h)))
    (t (error "undefined"))))

#+cover-unum (cover:annotate nil)

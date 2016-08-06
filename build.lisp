;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2016 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(push #P"../systems/" asdf:*central-registry*)
(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)
		   (compilation-speed 0)))
(funcall
 (sb-alien:define-alien-routine
     ("disable_lossage_handler" cl-user::disable-sbcl-ldb)
     sb-alien:void))
#+nil
(setf (sb-ext::bytes-consed-between-gcs) (* 4 1024 1024)
      (sb-ext::generation-number-of-gcs-before-promotion 0) 8
      (sb-ext::generation-number-of-gcs-before-promotion 1) 4
)
;;(setf *features* (remove :sb-ldb *features*))
(pushnew :cover-unum *features*)
(asdf:oos 'asdf:load-op :cl-unum)

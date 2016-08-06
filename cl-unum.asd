;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2016 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package #:asdf)

(defsystem #:cl-unum
  :depends-on ("cover" "rt")
  :components
  ((:file "unum-package")
   (:file "unum" :depends-on ("unum-package"))
   (:file "unum-test" :depends-on ("unum"))
   (:file "scratchpad" :depends-on ("unum"))
   (:file "scratchpad-test" :depends-on ("scratchpad"))
   (:file "ubox" :depends-on ("unum" "scratchpad"))
   (:file "ubox-test" :depends-on ("ubox"))))
;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2016 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :cl-user)

(defpackage #:unum
  (:use :cl)
  (:export
   #:debugize
   #:implies
   #:erp
   #:make-env
   #:*env*
   #:make-unum
   #:sign
   #:expo
   #:frac
   #:ubit
   #:e-size
   #:f-size
   #:ess
   #:fss
   #:copy-unum
   #:eqlu
   #:equalu
   #:diffu
   #:utag-size
   #:max-e-size
   #:max-f-size
   #:num-bits
   #:num-unums
   #:max-bits
   #:byte-mask
   #:ulp
   #:small-subnormal
   #:small-normal
   #:smallest
   #:biggest
   #:max-unum
   #:almost-zero-unum
   #:open-inf-unum
   #:almost-inf-unum
   #:lisp-inf
   #:lisp-biggest
   #:unum-inf
   #:unum-nan
   #:to-signum
   #:to-rational
   #:to-real
   #:+inf
   #:-inf
   #:inf-p
   #:nan-p
   #:exact
   #:inexact
   #:inexactp
   #:exactp
   #:lessp
   #:greaterp
   #:hnum
   #:unum
   #:ubound
   #:lo
   #:hi
   #:hi*
   #:make-ubound
   #:ubox
   #:ubarray
   #:make-ubarray
   #:relative-width
   #:*relative-width*
   #:expo-small
   #:frac-small
   #:calculate
   #:gnum
   #:make-gnum
   #:copy-gnum
   #:gbound
   #:make-gbound
   #:gbound-nan
   #:gbarray
   #:make-gbarray
   #:inc-e-size
   #:dec-e-size
   #:inc-f-size
   #:dec-f-size
   #:jointp
   #:disjointp
   #:unify+
   #:unify
   #:compress
   #:negate
   #:without-scratchpad
   ;; scratchpad
   #:reciprocate
   #:with-scratchpad
   #:*ubits-moved*
   #:*unums-moved*
   #:*ubounds-moved*
   #:plus
   #:minus
   #:times
   #:divide
   #:fma
   #:fam
   #:fdot
   #:fsum
   #:fprod
   #:fprod-ratio
   #:square
   #:sqroot
   #:reciprocate
   #:power
   #:e-power
   #:intersect
   #:poly
   #:absolute
   ;; ubox
   #:ulp-hi
   #:ulp-lo
   #:favor-e
   #:favor-f
   #:pad-frac
   #:nbor-hi
   #:nbor-lo
   #:find-nbors
   #:ubox-list
   #:inex-ubox-list
   #:split
   #:bisect
   #:leftmost
   #:rightmost
   #:unite
   #:insert
   #:solve-ulp))

(defpackage #:unum-user
  (:use :cl :unum :rtest))

;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-mongo-asd
  (:use :cl :asdf))

(in-package :weblocks-mongo-asd)

(defsystem weblocks-mongo
  :name "weblocks-mongo"
  :maintainer "Olexiy Zamkoviy"
  :author "Olexiy Zamkoviy"
  :version "0.0.1"
  :licence "LLGPL"
  :description "A weblocks backend for clsql."
  :depends-on (:cl-mongo :cl-mongo-id :weblocks-stores :weblocks-memory)
  :components ((:file "mongo")))


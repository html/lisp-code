
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-manardb-asd
  (:use :cl :asdf))

(in-package :weblocks-manardb-asd)

(defsystem weblocks-manardb
  :name "weblocks-manardb"
  :maintainer "Olexiy Zamkoviy"
  :author "Olexiy Zamkoviy"
  :version "0.1.0"
  :licence "LLGPL"
  :description "A weblocks backend for manardb."
  :depends-on (:metatilities :cl-ppcre :manardb :bordeaux-threads :weblocks-memory :weblocks-stores)
  :components ((:file "manardb")))

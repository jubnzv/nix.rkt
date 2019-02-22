#lang racket/base

(provide (all-defined-out))

;; Definitions from fcntl-linux.h
;; They could be different on other machines (but who cares now?)
(define	O_CREAT		64)

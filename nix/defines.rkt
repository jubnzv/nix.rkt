#lang racket/base

(provide (all-defined-out))

;; Definitions from fcntl-linux.h
(define O_RDONLY	#o0000)
(define O_WRONLY	#o0001)
(define O_RDWR		#o0002)
(define	O_CREAT   #o0100)
(define O_EXCL	  #o0200)

#lang racket/base

;; Functions to work with POSIX shared memory segments

(require ffi/unsafe
         ffi/unsafe/define
         racket/match)
(require "defines.rkt"
         "private/errors.rkt")

(provide shm-open
         shm-unlink)


(define-ffi-definer define-librt (ffi-lib "librt"))
(define-fun-syntax _errno
  (syntax-rules ()
    [(_ kind) (post: (saved-errno))]))


;; Foreign functions

(define-librt _shm-open (_fun #:save-errno 'posix
                             _string _int _int
                             -> _int)
  #:c-id shm_open)

(define-librt _shm-unlink (_fun #:save-errno 'posix
                               _string
                               -> _int)
  #:c-id shm_unlink)


;; Wrappers

(define (shm-open name oflag [mode #o644])
  (define shm-fd (_shm-open name oflag mode))
 (when (check shm-fd 'shm-open) shm-fd))

(define (shm-unlink name)
  (define res (_shm-unlink name))
  (if (zero? res) #t #f))

#lang racket/base

;; Provides functions to work with POSIX semaphores

(require ffi/unsafe
         ffi/unsafe/define
         racket/match)
(require "defines.rkt"
         "private/errors.rkt")

(provide sem-init
         sem-destroy
         sem-open
         sem-unlink
         sem-post
         sem-wait
         sem-trywait
         sem-getvalue)

(define-ffi-definer define-libpthread (ffi-lib "libpthread"))
(define-fun-syntax _errno
    (syntax-rules ()
      [(_ kind) (post: (saved-errno))]))


;; C types
(define-cpointer-type _sem_t)


;; Foreign functions

(define-libpthread sem-init (_fun #:save-errno 'posix
                                  (sem-p : (_ptr io _sem_t))
                                  _int
                                  _int
                                  -> (r : _int)
                                  -> (when (check r 'sem-init) sem-p))
  #:c-id sem_init)

(define-libpthread sem-destroy (_fun #:save-errno 'posix
                                      (sem : (_ptr i _sem_t))
                                      -> (r : _int)
                                      -> (when (check r 'sem-destroy) #t))
  #:c-id sem_destroy)

(define-libpthread _sem-open (_fun #:save-errno 'posix
                                  _path _int _int _int
                                  -> _sem_t)
#:c-id sem_open)

(define-libpthread sem-unlink (_fun #:save-errno 'posix
                                    _string
                                    -> (r : _int)
                                    -> (if (zero? r) #t #f))
  #:c-id sem_unlink)

(define-libpthread sem-wait (_fun #:save-errno 'posix
                                  _sem_t
                                  -> (r : _int)
                                  -> (when (check r 'sem-wait) #t))
  #:c-id sem_wait)

(define-libpthread sem-trywait (_fun #:save-errno 'posix
                                  (_ptr i _sem_t) ;; ???
                                  -> (r : _int)
                                  -> (when (check r 'sem-trywait) #t))
  #:c-id sem_trywait)

(define-libpthread sem-post (_fun #:save-errno 'posix
                                  _sem_t
                                  -> (r : _int)
                                  -> (when (check r 'sem-post) #t))
  #:c-id sem_post)

(define-libpthread sem-getvalue (_fun #:save-errno 'posix
                                      _sem_t
                                      (val : (_ptr o _int))
                                      -> (r : _int)
                                      -> (when (check r 'sem-getvalue) val))
  #:c-id sem_getvalue)


;; Wrappers

(define (sem-open name oflag
                  #:mode [mode #o644]
                  #:value [value 0])
  (define sem-p (_sem-open name oflag mode value))
  (when (check-null sem-p 'sem-open) sem-p))

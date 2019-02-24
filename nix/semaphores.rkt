#lang racket/base

;; Provides functions to work with POSIX semaphores

(require "defines.rkt")
(provide sem-init
         sem-destroy
         sem-open
         sem-unlink
         sem-post
         sem-wait
         sem-trywait
         sem-getvalue)

(require ffi/unsafe
         ffi/unsafe/define
         racket/match)

(define-ffi-definer define-libpthread (ffi-lib "libpthread"))


;; C types
;(define _sem_t (_cpointer/null 'sem_t))
(define-cpointer-type _sem_t)


;; Error handling helpers

(define strerror
  (get-ffi-obj 'strerror #f (_fun _int -> _bytes)))

(define-fun-syntax _errno
    (syntax-rules ()
      [(_ kind) (post: (saved-errno))]))

;; Handle non-zero retval
(define (check v who)
  (unless (zero? v)
    (let ([str (strerror (saved-errno))])
      (error who (bytes->string/locale str)))))

;; Handle NULL pointer retval
(define (check-null v who)
  ;(display v)
  (when (boolean? v)
    (let ([str (strerror (saved-errno))])
      (error who (bytes->string/locale str)))))


;; Foreign functions

(define-libpthread sem-init (_fun #:save-errno 'posix
                                  (sem : (_ptr io _sem_t))
                                  _int
                                  _int
                                  -> (r : _int)
                                  -> (when (check r 'sem-init) sem))
  #:c-id sem_init)

(define-libpthread sem-destroy (_fun #:save-errno 'posix
                                      (sem : (_ptr i _sem_t))
                                      -> (r : _int)
                                      -> (when (check r 'sem-destroy) #t))
  #:c-id sem_destroy)

(define-libpthread sem-open (_fun #:save-errno 'posix
                                  _path
                                  _int
                                  -> (r : _sem_t)
                                  -> (when (check-null r 'sem-open) r))
  #:c-id sem_open)

;; TODO: Handle expections
(define-libpthread sem-unlink (_fun #:save-errno 'posix
                                    _string
                                    -> (r : _int)
                                    -> r)
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
                                      -> (when (check r 'sem-getbalue) val))
  #:c-id sem_getvalue)

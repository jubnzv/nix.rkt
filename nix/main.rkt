#lang racket/base
;; Simple access to common UNIX APIs

(require "defines.rkt")
(provide (all-from-out "defines.rkt"))
(provide sem-open
         sem-unlink
         sem-post
         sem-wait
         sem-getvalue)

(require ffi/unsafe
         ffi/unsafe/define
         racket/match)

(define-ffi-definer define-libpthread (ffi-lib "libpthread"))

;; Racket value that reflects a C type via pointer
(define _sem_t-ptr (_cpointer/null 'sem_t))


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

(define-libpthread sem-open (_fun #:save-errno 'posix
                                  _path
                                  _int
                                  -> (r : _sem_t-ptr)
                                  -> (when (check-null r 'sem-open) r))
  #:c-id sem_open)

;; TODO: Handle expections
(define-libpthread sem-unlink (_fun #:save-errno 'posix
                                    _string
                                    -> (r : _int)
                                    -> r)
  #:c-id sem_unlink)

(define-libpthread sem-wait (_fun #:save-errno 'posix
                                  (_ptr i _sem_t-ptr) ;; ???
                                  -> (r : _int)
                                  -> (check r 'sem-wait))
  #:c-id sem_wait)

(define-libpthread sem-post (_fun #:save-errno 'posix
                                  _sem_t-ptr
                                  -> (r : _int)
                                  -> (check r 'sem-post))
  #:c-id sem_post)

(define-libpthread sem-getvalue (_fun #:save-errno 'posix
                                      _sem_t-ptr
                                      (val : (_ptr o _int))
                                      -> (r : _int)
                                      -> (values val r))
  #:c-id sem_getvalue)

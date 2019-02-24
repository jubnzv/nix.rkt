#lang racket/base

;; Error handling helpers

(require ffi/unsafe
         ffi/unsafe/define)

(provide check
         check-null)

(define strerror
  (get-ffi-obj 'strerror #f (_fun _int -> _bytes)))

;; Handle -1 return code
(define (check v who)
  (when (eq? v -1)
    (let ([str (strerror (saved-errno))])
      (error who (bytes->string/locale str)))))

;; Handle NULL pointer retval
(define (check-null v who)
  ;(display v)
  (when (boolean? v)
    (let ([str (strerror (saved-errno))])
      (error who (bytes->string/locale str)))))

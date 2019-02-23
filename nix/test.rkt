#lang racket

(require "main.rkt" rackunit)

;; Helper for test cases with multiple outputs
;; See: https://stackoverflow.com/questions/41081395/unit-testing-in-racket-with-multiple-outputs
(define-syntax check-values-equal?
  (syntax-rules ()
    [(_ a b) (check-equal? (call-with-values (thunk a) list) b)]))


;; Named POSIX semaphores
(test-begin
  (define test-sem-name "/test-nix-1")

  ;; Unlink if already exists
  (sem-unlink test-sem-name)

  ;; Open and unlink
  (define test-sem-p (sem-open test-sem-name (+ O_CREAT O_EXCL)))
  (check-not-false test-sem-p)
  (check-not-equal? test-sem-p (void))
  (check-exn exn:fail?
             (lambda () (sem-open test-sem-name O_CREAT))
             "Permission denied")
  (check-exn exn:fail?
             (lambda () (sem-open test-sem-name (+ O_CREAT O_EXCL))))

  ;; Change values
  (check-equal? (sem-getvalue test-sem-p) 0)
  (sem-post test-sem-p)
  (check-equal? (sem-getvalue test-sem-p) 1)
  (sem-wait test-sem-p)
  (check-equal? (sem-getvalue test-sem-p) 0)
  (sem-post test-sem-p)
  (check-equal? (sem-getvalue test-sem-p) 1)
  (sem-post test-sem-p)
  (check-equal? (sem-getvalue test-sem-p) 2)
  (sem-trywait test-sem-p)
  (check-equal? (sem-getvalue test-sem-p) 2)

  ;; Can't unlink twice
  (check-equal? (sem-unlink test-sem-name) 0)
  (check-equal? (sem-unlink test-sem-name) -1))

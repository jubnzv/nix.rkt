#lang racket

(require "main.rkt" rackunit)

;; Helper for test cases with multiple outputs
;; See: https://stackoverflow.com/questions/41081395/unit-testing-in-racket-with-multiple-outputs
(define-syntax check-values-equal?
  (syntax-rules ()
    [(_ a b) (check-equal? (call-with-values (thunk a) list) b)]))


;; POSIX semaphores tests
(define test-sem-name "/test-nix-1")

(test-begin
  ;; Unlink if already exists
  (sem-unlink test-sem-name)

  ;; Open
  (define test-sem-p (sem-open test-sem-name O_CREAT))
  (check-not-false test-sem-p)
  (check-not-equal? test-sem-p (void))
  (check-exn exn:fail?
             (lambda () (sem-open test-sem-name O_CREAT))
             "Permission denied")

  ;; Change values
  (check-values-equal? (sem-getvalue test-sem-p) '(0 0))
  (sem-wait test-sem-p)
  (check-values-equal? (sem-getvalue test-sem-p) '(0 0))
  (sem-post test-sem-p)
  (check-values-equal? (sem-getvalue test-sem-p) '(1 0))
  (sem-post test-sem-p)
  (check-values-equal? (sem-getvalue test-sem-p) '(2 0))

  (check-equal? (sem-unlink test-sem-name) 0))

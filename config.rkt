#lang racket/base

(provide
 *ports*
 *proxy-key*
 *secret*
 *http-flag*)

(define *configurations* (call-with-input-file "config.rktd" read))

(define *ports* (cdr (assoc 'ports *configurations*)))
(define *proxy-key* (cdr (assoc 'proxy-key *configurations*)))
(define *secret* (cdr (assoc 'secret *configurations*)))
(define *http-flag* (cdr (assoc 'http-flag *configurations*)))

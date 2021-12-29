#lang racket/base

(require racket/tcp
         racket/string
         rnrs/io/ports-6
         "cipher.rkt")

(provide
 handle-tcp-session)

(define (get-proxy-host header)
  (define re (regexp-match #px"Meng:\\s*(.*?)\\s*\r\n" header))
  (cond
    [(and re (>= (length re) 2))
     (bytes->string/utf-8 (decrypt-host! (cadr re)))]
    [else #f]))

(define (tcp-forward ip op)
  (let lp ([data (get-bytevector-some ip)]
           [subi 0])
    (unless (eof-object? data)
      (let ([rem (xor-cipher! data "quanyec" subi)])
        (put-bytevector op data)
        (flush-output-port op)
        (lp (get-bytevector-some ip) rem)))))

(define (handle-tcp-session ip op header)
  (define shost (get-proxy-host header))
  (printf "proxy host: ~a~n" shost)
  (cond
    [(not shost)
     (display "No proxy host" op)]
    [else
     (define-values (host port)
       (cond
         [(string-contains? shost ":")
          (define host-port (string-split shost ":"))
          (define p (cadr host-port))
          (values (car host-port) (string->number (substring p 0 (- (string-length p) 1))))]
         [else
          (values (values (substring shost 0 (- (string-length shost) 1)) 80))]))
     (define-values (dip dop) (tcp-connect host port))
     (thread (lambda () (tcp-forward ip dop)))
     (tcp-forward dip op)
     (close-input-port dip)
     (close-output-port dop)]))

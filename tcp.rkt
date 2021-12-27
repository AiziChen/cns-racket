#lang racket/base

(require racket/tcp
         racket/string
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
  (let lp ([bs (read-bytes 65536 ip)])
    (unless (eof-object? bs)
      (write-bytes bs op)
      (lp (read-bytes 65536 ip)))))

(define (handle-tcp-session ip op header)
  (define shost (get-proxy-host header))
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
     (thread (tcp-forward ip dop))
     (tcp-forward dip op)
     (close-input-port dip)
     (close-output-port dop)]))

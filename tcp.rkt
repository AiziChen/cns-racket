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
  (define bs (make-bytes 65536))
  (let lp ([subi 0])
    (define len (read-bytes-avail! bs ip))
    (unless (eof-object? len)
      (define abs (subbytes bs 0 len))
      (define rem (xor-cipher! abs "quanyec" subi))
      (write-bytes abs op)
      (when (= len 65536)
        (lp rem)))))

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
     (thread (lambda () (tcp-forward ip dop)))
     (tcp-forward dip op)
     (close-input-port dip)
     (close-output-port dop)]))

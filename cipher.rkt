#lang racket/base

(require racket/contract
         racket/string
         net/base64)

(provide
 xor-cipher!
 decrypt-host!)

(define/contract (xor-cipher! data secret subi)
  (-> bytes? non-empty-string? exact-integer? exact-integer?)
  (define data-len (bytes-length data))
  (define secret-len (string-length secret))
  (for/last ([i data-len])
    (define rem (remainder (+ subi i) secret-len))
    (bytes-set! data i
                (bitwise-xor (bytes-ref data i)
                             (bitwise-ior (char->integer (string-ref secret rem)) rem)))
    (+ rem 1)))


(define/contract (decrypt-host! host)
  (-> bytes? bytes?)
  (define de-host (base64-decode host))
  (xor-cipher! de-host "quanyec" 0)
  de-host)

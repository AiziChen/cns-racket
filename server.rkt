#lang racket/base

(require racket/tcp
         "tcp.rkt"
         "udp.rkt")

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (printf "Listen to port: ~a~n" port-no)
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener))
    (loop)
    (thread loop))
  (lambda () (custodian-shutdown-all main-cust)))


(define (accept-and-handle listener)
  (define cust (make-custodian))
  (custodian-limit-memory cust (* 50 1024 1024))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (printf "Handle a new connection~n")
    (thread (lambda ()
              (handle in out)
              (close-input-port in)
              (close-output-port out)))))

(define (handle in out)
  (define req
    ;; Match the first line to extract the request
    (regexp-match #rx"^(GET|POST|HEAD|PUT|COPY|DELETE|MOVE|OPTIONS|LINK|UNLINK|TRACE|PATCH|WRAPPED) (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (cond
    [req
     (define headers (regexp-match #rx".*?\r\n\r\n" in))
     (when headers
       (define header (car headers))
       (response-header out header)
       (unless (regexp-match #rx".*httpUDP.*" header)
         (printf "Start tcp forwarding...~n")
         (handle-tcp-session in out header)))]
    [else
     (handle-udp-session in out)]))


;;; Response Header
(define (response-header out headers)
  (cond
    [(regexp-match #rx".*WebSocket.*" headers)
     (display "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: CuteBi Network Tunnel, (%>w<%)\r\n\r\n" out)]
    [(regexp-match #rx".*CON.*" headers)
     (display "HTTP/1.1 200 Connection established\r\nServer: CuteBi Network Tunnel, (%>w<%)\r\nConnection: keep-alive\r\n\r\n" out)]
    [else
     (display "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nServer: CuteBi Network Tunnel, (%>w<%)\r\nConnection: keep-alive\r\n\r\n" out)]))


;;; Start the server
(define stop (serve 1080))

(with-handlers ([exn:break? (lambda (_) (stop))])
  (sync/enable-break))

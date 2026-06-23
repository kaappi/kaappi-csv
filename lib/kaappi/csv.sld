;;; (kaappi csv) — CSV parser and writer (RFC 4180)
;;;
;;; Mapping:
;;;   CSV file   → list of rows
;;;   CSV row    → list of strings (or vectors with csv-read headers: #t)
;;;   CSV field  → string
;;;
;;; Supports: quoted fields, embedded commas, embedded newlines,
;;; escaped quotes (""), custom delimiters, header row mode.

(define-library (kaappi csv)
  (import (scheme base) (scheme char) (scheme write))
  (export csv-read csv-read-string csv-read-row
          csv-write csv-write-string csv-write-row
          csv-fold)
  (begin

    ;; ---------------------------------------------------------------
    ;; Reader
    ;; ---------------------------------------------------------------

    (define (csv-read-string str . opts)
      (let ((port (open-input-string str)))
        (let ((result (apply csv-read port opts)))
          (close-input-port port)
          result)))

    (define (csv-read port . opts)
      (let ((delimiter (get-opt opts 'delimiter #\,))
            (headers?  (get-opt opts 'headers #f)))
        (if headers?
            (let ((header-row (csv-read-row port delimiter)))
              (if (not header-row)
                  '()
                  (let loop ((acc '()))
                    (let ((row (csv-read-row port delimiter)))
                      (if (not row)
                          (reverse acc)
                          (loop (cons (zip-to-alist header-row row) acc)))))))
            (let loop ((acc '()))
              (let ((row (csv-read-row port delimiter)))
                (if (not row)
                    (reverse acc)
                    (loop (cons row acc))))))))

    (define (csv-read-row port . args)
      (let ((delimiter (if (pair? args) (car args) #\,)))
        (let ((ch (peek-char port)))
          (if (eof-object? ch)
              #f
              (let loop ((fields '()))
                (let ((field (read-field port delimiter)))
                  (let ((next (peek-char port)))
                    (cond
                      ((eof-object? next)
                       (reverse (cons field fields)))
                      ((char=? next delimiter)
                       (read-char port)
                       (loop (cons field fields)))
                      ((char=? next #\return)
                       (read-char port)
                       (when (and (not (eof-object? (peek-char port)))
                                  (char=? (peek-char port) #\newline))
                         (read-char port))
                       (reverse (cons field fields)))
                      ((char=? next #\newline)
                       (read-char port)
                       (reverse (cons field fields)))
                      (else
                       (reverse (cons field fields)))))))))))

    (define (read-field port delimiter)
      (let ((ch (peek-char port)))
        (cond
          ((eof-object? ch) "")
          ((char=? ch #\") (read-quoted-field port))
          (else (read-unquoted-field port delimiter)))))

    (define (read-quoted-field port)
      (read-char port)
      (let loop ((acc '()))
        (let ((ch (read-char port)))
          (cond
            ((eof-object? ch) (list->string (reverse acc)))
            ((char=? ch #\")
             (let ((next (peek-char port)))
               (if (and (not (eof-object? next)) (char=? next #\"))
                   (begin (read-char port) (loop (cons #\" acc)))
                   (list->string (reverse acc)))))
            (else (loop (cons ch acc)))))))

    (define (read-unquoted-field port delimiter)
      (let loop ((acc '()))
        (let ((ch (peek-char port)))
          (cond
            ((eof-object? ch) (list->string (reverse acc)))
            ((char=? ch delimiter) (list->string (reverse acc)))
            ((char=? ch #\newline) (list->string (reverse acc)))
            ((char=? ch #\return) (list->string (reverse acc)))
            (else (read-char port) (loop (cons ch acc)))))))

    ;; ---------------------------------------------------------------
    ;; Fold (streaming — process rows without building full list)
    ;; ---------------------------------------------------------------

    (define (csv-fold port proc init . opts)
      (let ((delimiter (get-opt opts 'delimiter #\,))
            (headers?  (get-opt opts 'headers #f)))
        (if headers?
            (let ((header-row (csv-read-row port delimiter)))
              (if (not header-row)
                  init
                  (let loop ((acc init))
                    (let ((row (csv-read-row port delimiter)))
                      (if (not row)
                          acc
                          (loop (proc (zip-to-alist header-row row) acc)))))))
            (let loop ((acc init))
              (let ((row (csv-read-row port delimiter)))
                (if (not row)
                    acc
                    (loop (proc row acc))))))))

    ;; ---------------------------------------------------------------
    ;; Writer
    ;; ---------------------------------------------------------------

    (define (csv-write-string rows . opts)
      (let ((port (open-output-string)))
        (apply csv-write rows port opts)
        (get-output-string port)))

    (define (csv-write rows port . opts)
      (let ((delimiter (get-opt opts 'delimiter #\,)))
        (for-each
          (lambda (row)
            (csv-write-row row port delimiter))
          rows)))

    (define (csv-write-row row port . args)
      (let ((delimiter (if (pair? args) (car args) #\,)))
        (let loop ((fields (if (vector? row) (vector->list row) row))
                   (first? #t))
          (when (pair? fields)
            (unless first?
              (write-char delimiter port))
            (write-csv-field (car fields) port delimiter)
            (loop (cdr fields) #f)))
        (write-string "\r\n" port)))

    (define (write-csv-field val port delimiter)
      (let ((s (cond
                 ((string? val) val)
                 ((number? val) (number->string val))
                 ((boolean? val) (if val "true" "false"))
                 ((eq? val #f) "")
                 (else (let ((p (open-output-string)))
                         (write val p)
                         (get-output-string p))))))
        (if (needs-quoting? s delimiter)
            (begin
              (write-char #\" port)
              (let loop ((i 0))
                (when (< i (string-length s))
                  (let ((ch (string-ref s i)))
                    (when (char=? ch #\")
                      (write-char #\" port))
                    (write-char ch port))
                  (loop (+ i 1))))
              (write-char #\" port))
            (write-string s port))))

    (define (needs-quoting? s delimiter)
      (let loop ((i 0))
        (if (= i (string-length s))
            #f
            (let ((ch (string-ref s i)))
              (or (char=? ch delimiter)
                  (char=? ch #\")
                  (char=? ch #\newline)
                  (char=? ch #\return)
                  (loop (+ i 1)))))))

    ;; ---------------------------------------------------------------
    ;; Helpers
    ;; ---------------------------------------------------------------

    (define (zip-to-alist keys vals)
      (let loop ((ks keys) (vs vals) (acc '()))
        (if (or (null? ks) (null? vs))
            (reverse acc)
            (loop (cdr ks) (cdr vs)
                  (cons (cons (car ks) (car vs)) acc)))))

    (define (get-opt opts key default)
      (let loop ((rest opts))
        (cond
          ((null? rest) default)
          ((and (pair? rest) (pair? (cdr rest))
                (eq? (car rest) key))
           (cadr rest))
          ((pair? rest) (loop (cdr rest)))
          (else default))))))

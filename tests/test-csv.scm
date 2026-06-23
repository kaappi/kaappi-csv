(import (scheme base) (scheme write)
        (kaappi csv))

(define pass 0)
(define fail 0)

(define-syntax check
  (syntax-rules (=>)
    ((_ expr => expected)
     (let ((result expr) (exp expected))
       (if (equal? result exp)
           (set! pass (+ pass 1))
           (begin
             (set! fail (+ fail 1))
             (display "FAIL: ") (write 'expr)
             (display " => ") (write result)
             (display ", expected ") (write exp)
             (newline)))))))

;; --- Basic parsing ---

(display "Basic parsing\n")

(check (csv-read-string "a,b,c")
  => '(("a" "b" "c")))

(check (csv-read-string "a,b,c\n1,2,3")
  => '(("a" "b" "c") ("1" "2" "3")))

(check (csv-read-string "a,b,c\r\n1,2,3\r\n")
  => '(("a" "b" "c") ("1" "2" "3")))

;; --- Quoted fields ---

(display "Quoted fields\n")

(check (csv-read-string "\"hello\",world")
  => '(("hello" "world")))

(check (csv-read-string "\"has,comma\",ok")
  => '(("has,comma" "ok")))

(check (csv-read-string "\"has \"\"quotes\"\"\",ok")
  => '(("has \"quotes\"" "ok")))

(check (csv-read-string "\"line1\nline2\",ok")
  => '(("line1\nline2" "ok")))

;; --- Empty fields ---

(display "Empty fields\n")

(check (csv-read-string "a,,c")
  => '(("a" "" "c")))

(check (csv-read-string ",")
  => '(("" "")))

(check (csv-read-string "")
  => '())

;; --- Custom delimiter ---

(display "Custom delimiter\n")

(check (csv-read-string "a\tb\tc" 'delimiter #\tab)
  => '(("a" "b" "c")))

(check (csv-read-string "a;b;c" 'delimiter #\;)
  => '(("a" "b" "c")))

;; --- Headers mode ---

(display "Headers mode\n")

(let ((result (csv-read-string "name,age\nAlice,30\nBob,25" 'headers #t)))
  (check (length result) => 2)
  (check (assoc "name" (car result)) => '("name" . "Alice"))
  (check (assoc "age" (car result)) => '("age" . "30"))
  (check (assoc "name" (cadr result)) => '("name" . "Bob")))

;; --- Writing ---

(display "Writing\n")

(check (csv-write-string '(("a" "b" "c") ("1" "2" "3")))
  => "a,b,c\r\n1,2,3\r\n")

(check (csv-write-string '(("has,comma" "ok")))
  => "\"has,comma\",ok\r\n")

(check (csv-write-string '(("has \"quotes\"" "ok")))
  => "\"has \"\"quotes\"\"\",ok\r\n")

(check (csv-write-string '(("line1\nline2" "ok")))
  => "\"line1\nline2\",ok\r\n")

;; --- Numbers and booleans in output ---

(display "Type coercion in output\n")

(check (csv-write-string '((42 3.14 #t #f)))
  => "42,3.14,true,false\r\n")

;; --- Custom delimiter in output ---

(display "Custom delimiter output\n")

(check (csv-write-string '(("a" "b" "c")) 'delimiter #\tab)
  => "a\tb\tc\r\n")

;; --- Round-trip ---

(display "Round-trip\n")

(let* ((original '(("Name" "City" "Score")
                    ("Alice" "New York" "95")
                    ("Bob" "San Francisco" "87")
                    ("Charlie" "has,comma" "100")))
       (serialized (csv-write-string original))
       (parsed (csv-read-string serialized)))
  (check (length parsed) => 4)
  (check (car parsed) => '("Name" "City" "Score"))
  (check (cadddr parsed) => '("Charlie" "has,comma" "100")))

;; --- Fold ---

(display "Fold\n")

(let ((sum (csv-fold (open-input-string "1,2\n3,4")
                     (lambda (row acc)
                       (+ acc (string->number (cadr row))))
                     0)))
  (check sum => 6))

(let ((names (csv-fold (open-input-string "name,age\nAlice,30\nBob,25")
                       (lambda (row acc)
                         (cons (cdr (assoc "name" row)) acc))
                       '()
                       'headers #t)))
  (check names => '("Bob" "Alice")))

;; --- Summary ---

(newline)
(display pass) (display " passed, ")
(display fail) (display " failed\n")
(when (> fail 0) (exit 1))

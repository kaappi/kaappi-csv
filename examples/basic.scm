(import (kaappi csv))

(define data (csv-read-string
  "Name,City,Score\nAlice,New York,95\nBob,London,87\nCharlie,Tokyo,92"
  'headers #t))

(display "Top scorers:\n")
(for-each
  (lambda (row)
    (let ((name  (cdr (assoc "Name" row)))
          (city  (cdr (assoc "City" row)))
          (score (cdr (assoc "Score" row))))
      (display "  ")
      (display name)
      (display " (")
      (display city)
      (display "): ")
      (display score)
      (newline)))
  data)

(display "\nAs CSV:\n")
(display (csv-write-string
  '(("Product" "Price" "Stock")
    ("Widget" "9.99" "150")
    ("Gadget" "24.99" "42"))))

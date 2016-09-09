(defmodule leap
  (export all))

(defun leap-year (year)
  (or (and (== 0 (rem year 4))
           (/= 0 (rem year 100)))
      (== 0 (rem year 400))))

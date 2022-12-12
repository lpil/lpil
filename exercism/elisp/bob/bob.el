;;; bob.el --- Bob exercise (exercism)

;;; Commentary:

;;; Code:

(defun is-shouting (msg)
  (and (string-match "[A-Z]" msg)
       (string= (upcase msg) msg)))


(defun is-question (msg)
  (string-suffix-p "?" msg))


(defun is-silence (msg)
  (string-match "^ *$" msg))


(defun response-for (msg)
  (cond
   ((is-shouting msg) "Whoa, chill out!")
   ((is-question msg) "Sure.")
   ((is-silence msg) "Fine. Be that way!")
   (t "Whatever.")))


(provide 'bob)
;;; bob.el ends here

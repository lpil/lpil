;;; -*- lexical-binding: t -*-
;;; difference-of-squares.el --- Difference of Squares (exercism)

;;; Commentary:

;;; Code:

(require 'cl-lib)


(defun square (n)
  (* n n))


(defun sum (n)
  (cl-reduce #'+ n))


(defun range (n)
  (number-sequence 1 n))


(defun square-of-sums (n)
  (square (sum (range n))))


(defun sum-of-squares (n)
  (sum (mapcar #'square (range n))))


(defun difference (n)
  (- (square-of-sums n)
     (sum-of-squares n)))


(provide 'difference-of-squares)
;;; difference-of-squares.el ends here

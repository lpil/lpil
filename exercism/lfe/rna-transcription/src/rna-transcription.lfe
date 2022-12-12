(defmodule rna-transcription
  (export (to-rna 1)))

(defun to-rna
  ((#\G) #\C)
  ((#\C) #\G)
  ((#\T) #\A)
  ((#\A) #\U)
  ((bases) (when (is_list bases))
    (lists:map #'to-rna/1 bases)))

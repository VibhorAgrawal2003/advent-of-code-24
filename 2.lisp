;; Function to read input and count safe seq
(defun read-file-into-array (file-path)
  (with-open-file (stream file-path :direction :input)
    (let ((cnt 0) (A '()))
      (loop for line = (read-line stream nil)
        while line
        do
        (let ((numbers (mapcar #'parse-integer (split-sequence #\Space line))))
          (setq A (apply #'vector numbers))
          (when (validate A)
            (print A)
            (incf cnt))))
    (format t "~%~%Total safe sequences: ~d ~%" cnt)
    )
  )
)

;; Function to split a string by a single space
(defun split-sequence (delimiter string)
  (let ((tokens '()))
    (loop for start = 0 then (1+ end)
          for end = (position delimiter string :start start)
          do (push (subseq string start end) tokens)
          while end
          finally (push (subseq string start) tokens))
    (butlast (nreverse tokens))
  ))


;; Function to validate a report A by inc/dec order
(defun validate (A)
  (let ((increasing-flag t)
        (decreasing-flag t))
    (dotimes (i (1- (length A)))
      (let ((x (aref A i))
            (y (aref A (1+ i))))

        (if (not (and (< x y) (<= (- y x) 3)))
          (setq increasing-flag nil))

        (if (not (and (> x y) (<= (- x y) 3)))
          (setq decreasing-flag nil))))

    (or increasing-flag decreasing-flag)))

    
;; Call to the main function
(read-file-into-array "inputs/2.txt")



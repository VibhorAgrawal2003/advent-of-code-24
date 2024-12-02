;; Function to read input into 2 arrays
(defun read-file-into-arrays (file-path)
  (let ((A '())     ;Array A
        (B '()))    ;Array B

    (with-open-file (stream file-path :direction :input)
      (loop for line = (read-line stream nil)
            while line
            do
              (let* ((trimmed-line (string-trim '() line))
                     (values (split-sequence #\Space trimmed-line)))
                (when (and (first values) (second values))
                  (push (parse-integer (first values)) A)           ;Add first value to A
                  (push (parse-integer (second values)) B))))       ;Add second value to B
    
    ;; Return reversed lists since we used push
    (values (reverse A) (reverse B))))
)

;; Function to split a string by single space
(defun split-sequence (delimiter string)
  (let ((tokens '()))
    (loop for start = 0 then (1+ end)
          for end = (position delimiter string :start start)
          do (push (subseq string start end) tokens)
          while end
          finally (push (subseq string start) tokens))
    (nreverse tokens)))


;; Reading input values into arrays created here, A & B
(multiple-value-bind (A B) (read-file-into-arrays "inputs/1.txt")

  ;; PART-1
  (sort A #'<)
  (sort B #'<)
  (setq C (mapcar (lambda (a b) (abs (- a b))) A B ))           ;Absolute difference list
  (setq sum (reduce #'+ C))                                     ;Accumulate result into sum

  (format t "Total Distance of A & B: ~d ~%" sum)               ;Display final result

  ;; PART-2
  (let ((score 0))                                              ;Set score to 0
    (dolist (x A)                                               ;For each x in A
      (let ((cnt 0))                                            ;Set cnt to 0
        (dolist (y B)                                           ;For each y in B
          (when (= x y)                                         ;If x == y
            (incf cnt)                                          ;Increment cnt
          )
        )
        (setq score (+ score (* x cnt)))                        ;score += x*cnt
      )
    )
    (format t "Total Similarity of A & B: ~d ~%" score)         ;Display final result
  )
)

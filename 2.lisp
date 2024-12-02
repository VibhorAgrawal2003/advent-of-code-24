;; Function to read input and count safe seq
(defun read-file-into-array (file-path)
  (with-open-file (stream file-path :direction :input)
    (let ((cnt 0) (newcnt 0) (A '()))
      (loop for line = (read-line stream nil)
        while line
        do
        (let ((numbers (mapcar #'parse-integer (split-sequence #\Space line))))
          (setq A (apply #'vector numbers))
          (let ((safe (validate A)))
            (when safe
              (incf cnt))
            (when (not safe)
              (when (problem-dampener A)
                (incf newcnt)
              )
            )
          )))
    (format t "Total safe sequences: ~d ~%" cnt)
    (format t "Total safe sequences (with dampener): ~d ~%" (+ newcnt cnt))
    ))
)

;; Function to split a string by a single space
(defun split-sequence (delimiter string)
  (let ((tokens '()))
    (loop for start = 0 then (1+ end)
          for end = (position delimiter string :start start)
          do (push (subseq string start end) tokens)
          while end
          finally (push (subseq string start) tokens))
    (butlast (nreverse tokens)))
)


;; Function to remove an item from array
(defun remove-element-at-index (array index)
  (let ((new-array (make-array (1- (length array)))))
    (loop for i from 0 to (1- (length array)) do
      (unless (= i index)
        (setf (aref new-array (if (< i index) i (1- i)))
              (aref array i))))
    new-array)
)


;; Function to copy an array
(defun copy-array (arr)
  (let ((new-arr (make-array (array-dimension arr 0))))
    (dotimes (i (length arr))
      (setf (aref new-arr i) (aref arr i)))
    new-arr)
)


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

    (or increasing-flag decreasing-flag))
)


;; Function to dampen a report A that was unsafe
(defun problem-dampener (A)
  (let ((safe nil))
  
    (dotimes (i (length A))
      (let ((B (copy-array A)))
        (setq B (remove-element-at-index B i))
        (if (validate B)
          (setq safe t)
        )
      )
    )
    safe)
)


;; Call to the main function
(read-file-into-array "inputs/2.txt")



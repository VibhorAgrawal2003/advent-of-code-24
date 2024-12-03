;; Read file input into a string and return
(defun read-file-and-process (file-path)
  (let ((content ""))
    (with-open-file (stream file-path :direction :input)
      (loop for line = (read-line stream nil nil)
        while line
        do (setf content (concatenate 'string content line))))
  content))


;; Calculating result from input string
(defun calculate-result (content)
  (let ((buffer "") (n (length content)) (result 0) (a 0) (b 0) (s 0) (r 0))

    ;; Traverse entire string
    (loop for i from 0 below n
      do
      (setf buffer (concatenate 'string buffer (string (char content i))))

      ;; Look for a match
      (when (search "mul(" buffer)
        (setf buffer "")

        ;; Extract first value or keep it zero
        (let ((num 0))
          (loop for j from (+ i 1) below n
            do

            ;; Sequence for obtaining first value
            (if (digit-char-p (char content j))
              (progn ;then
              (incf s)
              (setf num (* num 10))
              (setf num (+ num (digit-char-p (char content j))))
              )

              (progn ;else
                (if (char= #\, (char content j))
                  (progn
                  (setf a num)
                  (setf num 0)
                  (return)
                  )
                  (return)
                )
              )
            )
            ;;end of loop
          )
        )

        ;; Extract second value or keep it zero
        (let ((num 0))
          (loop for j from (+ i s 2) below n
            do

            ;; Sequence for obtaining first value
            (if (digit-char-p (char content j))
              (progn ;then
              (incf r)
              (setf num (* num 10))
              (setf num (+ num (digit-char-p (char content j))))
              )

              (progn ;else
                (if (= (char-code (char content j)) 41)
                  (progn
                  (setf b num)
                  (setf num 0)
                  (return)
                  )
                  (return)
                )
              )
            )
            ;;end of loop
          )
        )
        (when (and (> a 0) (> b 0) (> s 0) (> r 0) (< s 4) (< r 4))
          (format t "~d ~d ~%" a b)
          (setq result (+ result (* a b)))
        )
        (setq s 0)
        (setq r 0)
        (setq a 0)
        (setq b 0)

        ;;end of match
      )
    )

    (format t "Final Result: ~d ~%" result)
  )
)



;; Calculating result from input string with do/dont instructions
(defun calculate-result-toggable (content)
  (let ((buffer "") (n (length content)) (result 0) (a 0) (b 0) (s 0) (r 0) (toggle t))

    ;; Traverse entire string
    (loop for i from 0 below n
      do
      (setf buffer (concatenate 'string buffer (string (char content i))))

      ;; Look for a do
      (when (search "do()" buffer)
        (setf toggle t)
      )

      ;; Look for a dont
      (when (search "don't()" buffer)
        (setf buffer "")
        (setf toggle nil)
      )

      ;; Look for a match
      (when (search "mul(" buffer)
        (setf buffer "")

        ;; Extract first value or keep it zero
        (let ((num 0))
          (loop for j from (+ i 1) below n
            do

            ;; Sequence for obtaining first value
            (if (digit-char-p (char content j))
              (progn ;then
              (incf s)
              (setf num (* num 10))
              (setf num (+ num (digit-char-p (char content j))))
              )

              (progn ;else
                (if (char= #\, (char content j))
                  (progn
                  (setf a num)
                  (setf num 0)
                  (return)
                  )
                  (return)
                )
              )
            )
            ;;end of loop
          )
        )

        ;; Extract second value or keep it zero
        (let ((num 0))
          (loop for j from (+ i s 2) below n
            do

            ;; Sequence for obtaining first value
            (if (digit-char-p (char content j))
              (progn ;then
              (incf r)
              (setf num (* num 10))
              (setf num (+ num (digit-char-p (char content j))))
              )

              (progn ;else
                (if (= (char-code (char content j)) 41)
                  (progn
                  (setf b num)
                  (setf num 0)
                  (return)
                  )
                  (return)
                )
              )
            )
            ;;end of loop
          )
        )
        (when (and (> a 0) (> b 0) (> s 0) (> r 0) (< s 4) (< r 4))
          (when toggle
            (format t "~d ~d ~%" a b)
            (setq result (+ result (* a b)))
          )
        )
        (setq s 0)
        (setq r 0)
        (setq a 0)
        (setq b 0)

        ;;end of match
      )
    )

    (format t "Final Result: ~d ~%" result)
  )
)



;; Driver code
(let ((content (read-file-and-process "inputs/3.txt")))
  (format t "~%PART-1~%")
  (calculate-result content)
  (format t "~%PART-2~%")
  (calculate-result-toggable content)
)


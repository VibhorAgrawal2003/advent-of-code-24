;; Read input file into a matrix
(defun read-file-into-matrix (file-path)
  (with-open-file (stream file-path :direction :input)
    (loop for line = (read-line stream nil)
      while line 
        collect (coerce line 'list)             ;Convert string into a char-list, collect in matrix
    )
  )
)


;; Count all occurences of XMAS in matrix
(defun count-xmas (A)
  (let ((m (length A)) (n (length (nth 0 A))) (count 0))
  
    ;; Count row-wise first
    (loop for i from 0 below m
      do
        (let ((x 1) (y 1))
        (loop for j from 0 below n
          do

            ;; Look for XMAS
            (if (and (= x 1) (char= #\X (nth j (nth i A))))
              (incf x)
              (if (and (= x 2) (char= #\M (nth j (nth i A))))
                (incf x)
                (if (and (= x 3) (char= #\A (nth j (nth i A))))
                  (incf x)
                  (if (and (= x 4) (char= #\S (nth j (nth i A))))
                    (progn
                    (setq x 1)
                    (incf count)
                    )
                    (if (char= #\X (nth j (nth i A)))
                      (setq x 2)
                      (setq x 1)
                    )
                  )
                )
              )
            )

            ;; Look for SAMX
            (if (and (= y 1) (char= #\S (nth j (nth i A))))
              (incf y)
              (if (and (= y 2) (char= #\A (nth j (nth i A))))
                (incf y)
                (if (and (= y 3) (char= #\M (nth j (nth i A))))
                  (incf y)
                  (if (and (= y 4) (char= #\X (nth j (nth i A))))
                    (progn
                    (setq y 1)
                    (incf count)
                    )
                    (if (char= #\S (nth j (nth i A)))
                      (setq x 2)
                      (setq x 1)
                    )                    
                  )
                )
              )
            )
        ))
    )

    ;; Count col-wise next
    (loop for j from 0 below n
      do
        (let ((x 1) (y 1))
        (loop for i from 0 below m
          do

            ;; Look for XMAS
            (if (and (= x 1) (char= #\X (nth j (nth i A))))
              (incf x)
              (if (and (= x 2) (char= #\M (nth j (nth i A))))
                (incf x)
                (if (and (= x 3) (char= #\A (nth j (nth i A))))
                  (incf x)
                  (if (and (= x 4) (char= #\S (nth j (nth i A))))
                    (progn
                    (setq x 1)
                    (incf count)
                    )
                    (if (char= #\X (nth j (nth i A)))
                      (setq x 2)
                      (setq x 1)
                    )                    
                  )
                )
              )
            )

            ;; Look for SAMX
            (if (and (= y 1) (char= #\S (nth j (nth i A))))
              (incf y)
              (if (and (= y 2) (char= #\A (nth j (nth i A))))
                (incf y)
                (if (and (= y 3) (char= #\M (nth j (nth i A))))
                  (incf y)
                  (if (and (= y 4) (char= #\X (nth j (nth i A))))
                    (progn
                    (setq y 1)
                    (incf count)
                    )
                    (if (char= #\S (nth j (nth i A)))
                      (setq x 2)
                      (setq x 1)
                    )                    
                  )
                )
              )
            )
        ))
    )

    ; ;; Count left-leaning diagonals
    ; (let ((p 0)) (loop for q from 0 below n do             ;Top-left point of diagonal
    ;     (let ((x 1) (y 1) (i p) (j q))
    ;       (loop while (and (< i m) (< j n))
    ;         do

    ;         ;; Look for XMAS
    ;         (if (and (= x 1) (char= #\X (nth j (nth i A))))
    ;           (incf x)
    ;           (if (and (= x 2) (char= #\M (nth j (nth i A))))
    ;             (incf x)
    ;             (if (and (= x 3) (char= #\A (nth j (nth i A))))
    ;               (incf x)
    ;               (if (and (= x 4) (char= #\S (nth j (nth i A))))
    ;                 (progn
    ;                 (setq x 1)
    ;                 (incf count)
    ;                 )
    ;                 (if (char= #\X (nth j (nth i A)))
    ;                   (setq x 2)
    ;                   (setq x 1)
    ;                 )                    
    ;               )
    ;             )
    ;           )
    ;         )

    ;         ;; Look for SAMX
    ;         (if (and (= y 1) (char= #\S (nth j (nth i A))))
    ;           (incf y)
    ;           (if (and (= y 2) (char= #\A (nth j (nth i A))))
    ;             (incf y)
    ;             (if (and (= y 3) (char= #\M (nth j (nth i A))))
    ;               (incf y)
    ;               (if (and (= y 4) (char= #\X (nth j (nth i A))))
    ;                 (progn
    ;                 (setq y 1)
    ;                 (incf count)
    ;                 )
    ;                 (if (char= #\S (nth j (nth i A)))
    ;                   (setq x 2)
    ;                   (setq x 1)
    ;                 )                    
    ;               )
    ;             )
    ;           )
    ;         )

    ;         ;; Update i, j
    ;         (incf i) (incf j)

    ;       ))
    ;   )
    ; )
    ; (let ((q 0)) (loop for p from 0 below m do             ;Top-left point of diagonal
    ;     (let ((x 1) (y 1) (i p) (j q))
    ;       (loop while (and (< i m) (< j n))
    ;         do

    ;         ;; Look for XMAS
    ;         (if (and (= x 1) (char= #\X (nth j (nth i A))))
    ;           (incf x)
    ;           (if (and (= x 2) (char= #\M (nth j (nth i A))))
    ;             (incf x)
    ;             (if (and (= x 3) (char= #\A (nth j (nth i A))))
    ;               (incf x)
    ;               (if (and (= x 4) (char= #\S (nth j (nth i A))))
    ;                 (progn
    ;                 (setq x 1)
    ;                 (incf count)
    ;                 )
    ;                 (if (char= #\X (nth j (nth i A)))
    ;                   (setq x 2)
    ;                   (setq x 1)
    ;                 )                    
    ;               )
    ;             )
    ;           )
    ;         )

    ;         ;; Look for SAMX
    ;         (if (and (= y 1) (char= #\S (nth j (nth i A))))
    ;           (incf y)
    ;           (if (and (= y 2) (char= #\A (nth j (nth i A))))
    ;             (incf y)
    ;             (if (and (= y 3) (char= #\M (nth j (nth i A))))
    ;               (incf y)
    ;               (if (and (= y 4) (char= #\X (nth j (nth i A))))
    ;                 (progn
    ;                 (setq y 1)
    ;                 (incf count)
    ;                 )
    ;                 (if (char= #\S (nth j (nth i A)))
    ;                   (setq x 2)
    ;                   (setq x 1)
    ;                 )                    
    ;               )
    ;             )
    ;           )
    ;         )

    ;         ;; Update i, j
    ;         (incf i) (incf j)

    ;       ))
    ;   )
    ; )

    ; ;; Count right-leaning diagonals
    ; (let ((p 0)) (loop for q from (- n 1) downto 0 do             ;Top-right point of diagonal
    ;     (let ((x 1) (y 1) (i p) (j q))
    ;       (loop while (and (< i m) (>= j 0))
    ;         do

    ;         ;; Look for XMAS
    ;         (if (and (= x 1) (char= #\X (nth j (nth i A))))
    ;           (incf x)
    ;           (if (and (= x 2) (char= #\M (nth j (nth i A))))
    ;             (incf x)
    ;             (if (and (= x 3) (char= #\A (nth j (nth i A))))
    ;               (incf x)
    ;               (if (and (= x 4) (char= #\S (nth j (nth i A))))
    ;                 (progn
    ;                 (setq x 1)
    ;                 (incf count)
    ;                 )
    ;                 (if (char= #\X (nth j (nth i A)))
    ;                   (setq x 2)
    ;                   (setq x 1)
    ;                 )                    
    ;               )
    ;             )
    ;           )
    ;         )

    ;         ;; Look for SAMX
    ;         (if (and (= y 1) (char= #\S (nth j (nth i A))))
    ;           (incf y)
    ;           (if (and (= y 2) (char= #\A (nth j (nth i A))))
    ;             (incf y)
    ;             (if (and (= y 3) (char= #\M (nth j (nth i A))))
    ;               (incf y)
    ;               (if (and (= y 4) (char= #\X (nth j (nth i A))))
    ;                 (progn
    ;                 (setq y 1)
    ;                 (incf count)
    ;                 )
    ;                 (if (char= #\S (nth j (nth i A)))
    ;                   (setq x 2)
    ;                   (setq x 1)
    ;                 )                    
    ;               )
    ;             )
    ;           )
    ;         )

    ;         ;; Update i, j
    ;         (incf i) (decf j)

    ;       ))
    ;   )
    ; )
    ; (let ((q (- n 1))) (loop for p from 0 below m do             ;Top-right point of diagonal
    ;     (let ((x 1) (y 1) (i p) (j q))
    ;       (loop while (and (< i m) (>= j 0))
    ;         do

    ;         ;; Look for XMAS
    ;         (if (and (= x 1) (char= #\X (nth j (nth i A))))
    ;           (incf x)
    ;           (if (and (= x 2) (char= #\M (nth j (nth i A))))
    ;             (incf x)
    ;             (if (and (= x 3) (char= #\A (nth j (nth i A))))
    ;               (incf x)
    ;               (if (and (= x 4) (char= #\S (nth j (nth i A))))
    ;                 (progn
    ;                 (setq x 1)
    ;                 (incf count)
    ;                 )
    ;                 (if (char= #\X (nth j (nth i A)))
    ;                   (setq x 2)
    ;                   (setq x 1)
    ;                 )                    
    ;               )
    ;             )
    ;           )
    ;         )

    ;         ;; Look for SAMX
    ;         (if (and (= y 1) (char= #\S (nth j (nth i A))))
    ;           (incf y)
    ;           (if (and (= y 2) (char= #\A (nth j (nth i A))))
    ;             (incf y)
    ;             (if (and (= y 3) (char= #\M (nth j (nth i A))))
    ;               (incf y)
    ;               (if (and (= y 4) (char= #\X (nth j (nth i A))))
    ;                 (progn
    ;                 (setq y 1)
    ;                 (incf count)
    ;                 )
    ;                 (if (char= #\S (nth j (nth i A)))
    ;                   (setq x 2)
    ;                   (setq x 1)
    ;                 )                    
    ;               )
    ;             )
    ;           )
    ;         )

    ;         ;; Update i, j
    ;         (incf i) (decf j)

    ;       ))
    ;   )
    ; )

    ;; return-value
    count
  )
)


;; Example usage:
(let ((A (read-file-into-matrix "inputs/4.txt")))
  (format t "Occurences of XMAS: ~d ~%" (count-xmas A))
)

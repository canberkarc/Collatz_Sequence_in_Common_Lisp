; Function to find Collatz Sequence according to given number
(defun collatz (n)
  (let ((res (loop collect n while (> n 1) 
                   do (setf n (if (oddp n)
                                  (1+ (* 3 n))
                                  (/ n 2))))))
    (values res (apply #'+ res))))

; Function to store lines into list
(defun lines-into-list (name)
  (with-open-file (in name)
    (loop for l = (read-line in nil nil)
      while l
      collect l)))

; Function to read objects from lines as list
(defun obj-from-lines (line)
  (read-from-string (concatenate 'string "(" line ")")))

; Function to flatten nested lists
(defun flattener (L)
   (cond
      ((not (listp L)) L)
      ((null L) L)
      (
        (not (listp (car L))) (cons (car L) (flattener (cdr L))))
      (t (append (flattener (car L)) (flattener (cdr L)))))) 

; Function to read integers from input file and flatten nested list if there is any.
(defun read-and-flatten (name)
  (defvar lst
  	(flattener
  		(mapcar #'obj-from-lines 
  			(lines-into-list name)
  		)))
)

; Functions to split string with given seperators
(defun splitf (string &optional (separator sep) (cns nil)) 
    (let ((n (position separator string
    :from-end t
        :test #'(lambda (x y)(find y x :test #'string=)))))
      (if n
        (splitf (subseq string 0 n) separator (cons (subseq string (1+ n)) cns))
        (cons string cns))
  )
)

(defun splits (string &optional (separator sep))
  (splitf string separator)
)

; Function to write collatz sequences with inputs to the output file
(defun write-to-file ()
  (with-open-file 
   (stream "collatz_outputs.txt" ; Output file 
       :direction :output    
       :if-exists :overwrite ; Overwrite if file exists
       :if-does-not-exist :create) ; Create if file doesn't exist
    (read-and-flatten "integer_inputs.txt") ; Input file
    (setf cnt 0)
    (loop for i in lst
  		do(format stream "~%~s: " (read-from-string (write-to-string i))) ; Input number
  		(format stream "~d " (cadr(splits (write-to-string(collatz i)) "()"))) ; Input number's Collatz Sequence
  	 (setf cnt (+ cnt 1))
     (when (= cnt 5)
      (return))
    )
  )
)

(write-to-file)
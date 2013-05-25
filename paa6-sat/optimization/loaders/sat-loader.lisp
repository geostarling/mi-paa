;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: knapsack.lisp

;;;; The SAT Problem Loader

(declaim (optimize (speed 0) (safety 0) (debug 3)))


(defun get-sat-instance (name)
  (let ((clauses nil)
	(weights nil))
    (with-open-file (input name)
      (loop for line = (read-line input nil)
	 while line do 
	   (let ((splitted-line (string-split line)))
	     (if (not (or (string= (car splitted-line) "c") 
			  (string= (car splitted-line) "p")))
		 (if (string= (car splitted-line) "%")
		     (progn 
		       (read-line input nil)
		       (read-line input nil)
		       
		       (let ((wline (read-line input nil)))
			 (setf weights (map 'list  #'parse-integer (string-split wline)))))
		     (progn
		       (if (string= (car splitted-line) "")
			   (setf splitted-line (cdr splitted-line)))
		       (let ((clause (loop for token in splitted-line
					until (string= token "0")
					collect (parse-integer token)
					  )))
			 (setf clauses (cons clause clauses))
			 )))))))
    (make-sat name clauses weights)
    ))

(defun get-lit-from-list (lit-idx list)
  (if (not list)
      nil
      (if (= lit-idx (literal-index (car list)))
	  (car list)
	  (get-lit-from-list lit-idx (cdr list)))))

  

(defun make-sat (name clauses weights)
  (let ((literals-res nil)
	(clauses-res nil))
    (loop 
       for w in weights
       for i from 1 to (length weights)
       do
	 (setf literals-res (cons (make-literal :index (- i 1) :weight w) literals-res)))
    (dolist (cl clauses)
      (let ((current-lits nil))
	(dolist (lit-idx cl)
	  (let ((current-lit (copy-literal (get-lit-from-list (- (abs lit-idx) 1) literals-res))))
	    (if (< lit-idx 0)
		(setf (literal-neg current-lit) T)
		(setf (literal-neg current-lit) NIL))
	    (setf current-lits (cons current-lit current-lits)))
	  )
	(setf clauses-res (cons (make-clause :literals current-lits) clauses-res))))
    (make-sat-problem :id name :literals (reverse literals-res) :clauses clauses-res)))




(defun load-sat (path)
  
   (get-sat-instance path)
)


(defun load-sat-all (datasets)
  (let ((res nil))
    (dolist (ds datasets)
      (setf res (cons (load-sat ds) res))
      )
    res))
  


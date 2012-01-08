;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: knapsack.lisp

;;;; The Knapsack Problem Loader



(defun string-split (string)
  (loop :for start := 0 :then (1+ finish)
        :for finish := (position #\Space string :start start)
        :collecting (subseq string start finish)
        :until (null finish)))


(defun proc-items (items-str-list items) 
  (if (not items-str-list)
      items
      (proc-items (cddr items-str-list) 
		  (cons 
		   (make-item :weight (parse-integer (car items-str-list))
			      :price (parse-integer (cadr items-str-list)))
		   items))))

(defun get-instances-iter (in insts)
  (let ((line (read-line in nil)))
    (if line 
	(let ((inst-list (string-split line)))
	  (get-instances-iter in 
			      (cons 
			       (make-knapsack-problem
				:id (parse-integer (car inst-list))
				:capacity (parse-integer (caddr inst-list))
				:items (reverse (proc-items (cdddr inst-list) nil)))
			       insts)))
	insts)))


(defun get-instances (in)
  (get-instances-iter in nil))


(defun load-knapsack (path)
  (let ((in (open path :if-does-not-exist nil)))
    (when in
      (prog1 
	  (get-instances in)
	(close in)))))

(defun load-all ()
  (load-knapsack "data/knap_10.inst.dat"))


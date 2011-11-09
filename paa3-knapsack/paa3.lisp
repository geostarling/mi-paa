(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun string-split (string)
  (loop :for start := 0 :then (1+ finish)
        :for finish := (position #\Space string :start start)
        :collecting (subseq string start finish)
        :until (null finish)))


(defstruct knapsack id n capacity items)
(defstruct result solution step-counter start-time end-time)  

    
(defun proc-items (items-str-list items) 
  (if (not items-str-list)
      items
      (proc-items (cddr items-str-list) (cons (cons (parse-integer (car items-str-list)) (parse-integer (cadr items-str-list))) items))))

(defun get-instances-iter (in insts)
  (let ((line (read-line in nil)))
    (if line 
	(let ((inst-list (string-split line)))
	  (get-instances-iter in 
			      (cons 
			       (make-knapsack 
				:id (parse-integer (car inst-list))
				:n (parse-integer (cadr inst-list))
				:capacity (parse-integer (caddr inst-list))
				:items (proc-items (cdddr inst-list) nil))
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
  (load-knapsack "data/knap_40.inst.dat"))

(defun load-first () 
  (let ((knap (car (load-all))))
    (setf (knapsack-items knap) (reverse (knapsack-items knap)))
    knap))

(defun get-price (config knap)
  (get-price-iter config (knapsack-items knap) 0))

(defun get-price-iter (config knap-items price-sum)
  (if (car config)
      (if (eql (car config) 1) 
	  (get-price-iter (cdr config) (cdr knap-items) (+ price-sum (cdar knap-items)))
	  (get-price-iter (cdr config) (cdr knap-items) price-sum))
      price-sum))

(defun is-overweight? (config knap)
  (is-overweight?-iter config (knapsack-items knap) (knapsack-capacity knap) 0))

(defun is-overweight?-iter (config knap-items knap-capacity weight-sum)
  (if (not (car config))
      NIL ; we reached end of list so we did not outreach knap capacity
      (if (eql (car config) 1) 
	  (let ((new-weight-sum (+ weight-sum (caar knap-items))))
	    (if (> new-weight-sum knap-capacity)
		T
		(is-overweight?-iter (cdr config)
				     (cdr knap-items)
				     knap-capacity
				     new-weight-sum)))
	  (is-overweight?-iter (cdr config)
			       (cdr knap-items)
			       knap-capacity
			       weight-sum))))

(defun bb-algorithm (knapsack)
;; takes knapsack and returns bestConfiguration
  (let ((stack nil))
    (push '(0) stack)
    (push '(1) stack)
    (bb-algorithm-optimize knapsack stack (make-result :solution NIL :step-counter 0))
    ))
  

(defun bb-algorithm-optimize (knap stack res)
  (let ((stack-top (pop stack)))    
    ;(print "Top of stack contains: ")
    ;(prin1 stack-top)
    (incf (result-step-counter res))
    (cond ((not stack-top) res)
	  ((is-overweight? stack-top knap) (bb-algorithm-optimize knap 
								  stack 
								  res))
	  (T (let ((child-states (get-child-states 
				  stack-top 
				  (list-length (knapsack-items knap)))))
	       (when (> (get-price stack-top knap) (get-price (result-solution res) knap))
		 (setf (result-solution res) stack-top))
	       (if (not child-states)
		   (bb-algorithm-optimize knap stack res)
		   (progn
		     (when (> (get-max-reachable-price (car child-states) knap)
			      (get-price (result-solution res) knap))
		       (push (car child-states) stack))  
		     (when (> (get-max-reachable-price (cadr child-states) knap)
			      (get-price (result-solution res) knap))
		       (push (cadr child-states) stack))
		     (bb-algorithm-optimize knap stack res))))))))

(defun get-child-states (config num-items)
  (if (eql (list-length config) num-items)
      nil
      (list (concatenate 'list config '(0))
	    (concatenate 'list config '(1)))))

(defun get-max-reachable-price (config knap)
  (+ (get-price config knap) (apply #'+ (mapcar #'cdr (nthcdr (list-length config) (knapsack-items knap))))))


;================================================================================

(defun dyn-algorithm (knap)
  (let ((memory-arr (make-array 
		     (list (1+ (knapsack-capacity knap))
			   (1+ (list-length (knapsack-items knap)))) :initial-element 0))
	(knap-items (knapsack-items knap))
	(capacity (knapsack-capacity knap)))
;    (break)
    (loop for i from 1 below (1+ (list-length (knapsack-items knap))) do
	 (loop for w from 0 upto capacity do 
	      (let ((item-weight (caar knap-items)) 
		    (item-price (cdar knap-items)))		
		;(break)	
		(format t "~% ============== ~% Loop values are: ~% W: ~D ~% I: ~D ~% ITEM-WEIGHT: ~D ~% ITEM-PRICE: ~D ~%" W I ITEM-WEIGHT ITEM-PRICE)
		(if (<= item-weight w)
		    (if (> 
			 (+ item-price (aref memory-arr (- w item-weight) (1- i)))
			 (aref memory-arr w (1- i)))
			(progn
			  ;(break)
			  (setf (aref memory-arr w i) 
				(+ item-price (aref memory-arr (- w item-weight) (1- i)))))
			(progn
			  ;(break)			  
			  (setf (aref memory-arr w i) 
				(aref memory-arr w (1- i)))))
		    (progn
		      ;(break)
		      (setf (aref memory-arr w i) 
			    (aref memory-arr w (1- i)))))
		;(show-board memory-arr)
		))
	 (setf knap-items (cdr knap-items)))
    (make-result :solution (dyn-get-solution memory-arr) :step-counter 0)))



(defun dyn-get-solution (mem-arr)
  (let ((result-sol nil) 
	(prev-value 0)
	(last-row-idx (1- (array-dimension mem-arr 0)))
	(last-col-idx (1- (array-dimension mem-arr 1))))
    (loop for i from 1 upto last-col-idx do
	 (if (= (aref mem-arr last-row-idx i) prev-value)
	     (setf result-sol (cons 0 result-sol))
	     (setf result-sol (cons 1 result-sol)))
	 (setf prev-value (aref mem-arr last-row-idx i)))
    (reverse result-sol)))


(defun show-board (board)
  (loop for i below (car (array-dimensions board)) do
       (loop for j below (cadr (array-dimensions board)) do
          (let ((cell (aref board i j)))
            (format t "~a " cell)))
       (format t "~%")))


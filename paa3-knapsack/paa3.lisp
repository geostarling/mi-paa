(declaim (optimize (speed 3) (safety 3) (debug 0)))

(defun string-split (string)
  (loop :for start := 0 :then (1+ finish)
        :for finish := (position #\Space string :start start)
        :collecting (subseq string start finish)
        :until (null finish)))


(defstruct knapsack id n capacity items)
(defstruct result id solution price step-counter (start-time 0) (end-time 0))  

    
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

(defun load-first () 
  (let ((knap (car (load-all))))
    (setf (knapsack-items knap) (knapsack-items knap))
    knap))

(defun get-price (config knap)
  (apply #'+ (mapcar #'(lambda (x y) (* x (cdr y))) config (knapsack-items knap))))

(defun get-weight (config knap)
  (apply #'+ (mapcar #'(lambda (x y) (* x (car y))) config (knapsack-items knap))))

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
    (bb-algorithm-optimize knapsack stack (make-result :id (knapsack-id knapsack) :solution NIL :price 0 :step-counter 0))
    ))
  

(defun bb-algorithm-optimize (knap stack res)
  (let ((stack-top (pop stack)))    
    ;(print "Top of stack contains: ")
    ;(prin1 stack-top)
    (incf (result-step-counter res))
    (if (not stack-top) 
	(progn 
	  (setf (result-solution res) (result-solution res))
	  (setf (result-price res) (get-price (result-solution res) knap))
	  res)
	(let ((child-states (get-child-states 
			     stack-top 
			     (list-length (knapsack-items knap)))))
	  (when (not (is-overweight? stack-top knap))
	      (when (> (get-price stack-top knap) (get-price (result-solution res) knap))
		(setf (result-solution res) stack-top))
	      (when child-states
		(when (> (get-max-reachable-price (car child-states) knap)
			 (get-price (result-solution res) knap))
		  (push (car child-states) stack))  
		(when (> (get-max-reachable-price (cadr child-states) knap)
			 (get-price (result-solution res) knap))
		  (push (cadr child-states) stack))))
	  (bb-algorithm-optimize knap stack res)))))


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
		;(format t "~% ============== ~% Loop values are: ~% W: ~D ~% I: ~D ~% ITEM-WEIGHT: ~D ~% ITEM-PRICE: ~D ~%" W I ITEM-WEIGHT ITEM-PRICE)
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
    (show-board memory-arr)
    (make-result :id (knapsack-id knap) :solution (dyn-get-solution memory-arr) :step-counter 0)))



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



(defun dyn-td-algorithm (knap)
  (let ((memory (make-hash-table :test 'equal))
	(result (make-result :id (knapsack-id knap) :step-counter 0)))

    (dyn-td-algorithm-iter (knapsack-items knap) (knapsack-capacity knap) memory result)
;    (print price)
    (setf (result-solution result) (get-dyn-td-results knap memory))
    (setf (result-price result) (get-price (result-solution result) knap))
 ;   (print memory)
    result))

(defun dyn-td-algorithm-iter (items capacity memory result)
  (let* (
	 (price nil)
	 (item (car items))
	 (item-weight (caar items))
	 (item-price (cdar items))
	 (mem-result (gethash (list item capacity) memory)) ; may be NIL!
	 )
    (cond 
      ((not items) 
       (setf price 0))
      (mem-result
       (setf price mem-result))
      ((> item-weight capacity)
       (setf price (dyn-td-algorithm-iter (cdr items) capacity memory result)))
      (T
       (setf price (max 
		    (+ item-price (dyn-td-algorithm-iter (cdr items) (- capacity item-weight) memory result))
		    (dyn-td-algorithm-iter (cdr items) capacity memory result)))))
    (setf (gethash (list item capacity) memory) price)
    (incf (result-step-counter result))
    price))


(defun price (item)
  (cdr item))

(defun weight (item)
  (car item))

; ======================== MEMORY =========================


(defun mload (mem i c)
  (if (< c 0) 
      'infinity
      (aref mem i c)))

(defun mstore (mem i c val)
  (setf (aref mem i c) val))

(defun get-i-dim (mem) 
  (array-dimension mem 0))

(defun get-c-dim (mem) 
  (array-dimension mem 1))



(defun sum (list)
  (apply #'+ list))

(defun minimum (a b)
  (if (lesser a b) a b))


(defun lesser (a b)
;  (print "lesser")
;  (print a)
;  (print b)
  (cond
    ((eql a 'infinity) nil)
    ((eql b 'infinity) t)
    (t (< a b))))

(defun get-lesser (a b)
  (if (lesser a b) a b))


(defun dyn-price-alg (knap &optional real-knap)
  ;(print knap)
  (let* ((items (knapsack-items knap))
	 (i-dim (1+ (length items)))
	 (c-dim (1+ (sum (mapcar #'price (knapsack-items knap)))))
	 (mem (make-array `(,i-dim ,c-dim) :initial-element 'infinity))
	 (steps 0))
    (loop for i from 0 below i-dim do 
	 (setf (aref mem i 0) 0))
    ;(print "cdim")
    ;(print c-dim)
    ;(print items)

    (loop
       for i from 1 below i-dim
       do
	 (loop 
	    for c from 0 below c-dim
	    do
	      (let ((item (nth (1- i) items)))
		;(print item)
		;(print i)
		;(print c)
		;(print mem)
;		(if (and (= i 5) (= c 64)) (break))
;		(break)
		;(print (plus (mload mem i (- c (price item))) (weight item)))
		(incf steps 1)
		(mstore mem 
			i
			c 
			(minimum (mload mem (1- i) c)
				 (plus (mload mem (1- i) (- c (price item))) (weight item)))))))
    ;(print "res")
    ;(print (find-result-price mem (knapsack-capacity knap)))
    ;(print mem)
;    (print "steps")
;'    (print steps)
;    (break)
    (let ((result (collect-result mem items (knapsack-capacity knap))))
      (make-result :id (knapsack-id knap) :solution result :step-counter 0 :price (get-price result (if real-knap real-knap knap))))))


(defun is-in-result (mem i c)
  (not (eql (mload mem i c) (mload mem (1- i) c))))


(defun collect-result-iter (mem items res i current-c)
  (if (= i 0)
      res
      (collect-result-iter 
	 mem 
	 items
	 (cons (if (is-in-result mem i current-c)
		   1
		   0) 
	       res)
	 (1- i) 
	 (- current-c 
	    (if (is-in-result mem i current-c)
		(price (nth (1- i) items))
		0)))))

(defun find-result-price (mem capacity)
  (let ((res-weight 'infinity)
	(i-dim (get-i-dim mem))
	(res-price 0))
    
    ;(print "critical")
    ;(print (1- (get-i-dim mem)))
    (loop 
       for c from 1 below (get-c-dim mem)
       do
	 (if (lesser (mload mem (1- i-dim) c) capacity)
	     (progn
	       (setf res-weight  (mload mem (1- i-dim) c))
	       (setf res-price c))))
    res-price))

(defun res-price (res) (car res))

(defun res-weight (res) (cdr res))

(defun collect-result (mem items capacity)  
  (collect-result-iter mem items nil (1- (get-i-dim mem)) (find-result-price mem capacity)))





(defun plus (opd1 opd2)
  (if (or (eql opd1 'infinity) (eql opd2 'infinity)) 
      'infinity
      (+ opd1 opd2)))


(defun get-dyn-td-results (knapsack memory)
  (reverse (get-dyn-td-results-iter 
   (knapsack-items knapsack) 
   (knapsack-capacity knapsack) 
   memory 
   nil))
  )

(defun get-dyn-td-results-iter (items capacity memory solution)
  (let* ((item (car items))
	 (item-weight (car item))	 
	 (next-item (cadr items)))
    (cond 
      ((not items) 
       solution)
      ((= (gethash (list item capacity) memory) (gethash (list next-item capacity) memory))
       (setf solution (cons 0 solution))
       (get-dyn-td-results-iter (cdr items) capacity memory solution))
      (T 
       (setf solution (cons 1 solution))
       (get-dyn-td-results-iter (cdr items) (- capacity item-weight) memory solution))
      )))

(defun approximate-knapsack-weights (knap ratio) 
  (mapcar #'(lambda (x) 
	      (setf (car x) 
		    (if (> (ash (car x) (- ratio)) 0) 
			(ash (car x) (- ratio)) 
			1))) 
	  (knapsack-items knap)) 
  (setf (knapsack-capacity knap) (ash (knapsack-capacity knap) (- ratio)))
  knap)


;;================================================================================

(defstruct dataset n path)

(defun get-datasets ()
  (reverse 
   (list
;    (make-dataset :n 40 :path "data/knap_40.inst.dat")
;    (make-dataset :n 37 :path "data/knap_37.inst.dat")
;    (make-dataset :n 35 :path "data/knap_35.inst.dat")
;    (make-dataset :n 32 :path "data/knap_32.inst.dat")
;    (make-dataset :n 30 :path "data/knap_30.inst.dat")
;    (make-dataset :n 27 :path "data/knap_27.inst.dat")
;    (make-dataset :n 25 :path "data/knap_25.inst.dat")
;    (make-dataset :n 22 :path "data/knap_22.inst.dat")
;    (make-dataset :n 20 :path "data/knap_20.inst.dat")
;    (make-dataset :n 15 :path "data/knap_15.inst.dat")
;    (make-dataset :n 10 :path "data/knap_10.inst.dat"))))
;   (make-dataset :n 4 :path "data/knap_4.inst.dat"))))
;   (make-dataset :n 4 :path "data/knap_test.inst.dat"))))



    (make-dataset :n 24 :path "data/max-weight/knap_40.inst.dat")
    (make-dataset :n 24 :path "data/max-weight/knap_50.inst.dat")
    (make-dataset :n 24 :path "data/max-weight/knap_60.inst.dat")
    (make-dataset :n 24 :path "data/max-weight/knap_100.inst.dat")
    (make-dataset :n 24 :path "data/max-weight/knap_150.inst.dat")
    (make-dataset :n 24 :path "data/max-weight/knap_210.inst.dat")
    (make-dataset :n 24 :path "data/max-weight/knap_280.inst.dat")
    (make-dataset :n 24 :path "data/max-weight/knap_360.inst.dat")
    (make-dataset :n 24 :path "data/max-weight/knap_460.inst.dat")
    (make-dataset :n 24 :path "data/max-weight/knap_600.inst.dat")
    (make-dataset :n 24 :path "data/max-weight/knap_700.inst.dat")

    (make-dataset :n 24 :path "data/max-price/knap_20.inst.dat")
    (make-dataset :n 24 :path "data/max-price/knap_40.inst.dat")
    (make-dataset :n 24 :path "data/max-price/knap_80.inst.dat")
    (make-dataset :n 24 :path "data/max-price/knap_100.inst.dat")
    (make-dataset :n 24 :path "data/max-price/knap_140.inst.dat")
    (make-dataset :n 24 :path "data/max-price/knap_180.inst.dat")
    (make-dataset :n 24 :path "data/max-price/knap_240.inst.dat")
    (make-dataset :n 24 :path "data/max-price/knap_300.inst.dat")
    (make-dataset :n 24 :path "data/max-price/knap_380.inst.dat")
    (make-dataset :n 24 :path "data/max-price/knap_400.inst.dat")
    (make-dataset :n 24 :path "data/max-price/knap_500.inst.dat")
    (make-dataset :n 24 :path "data/max-price/knap_600.inst.dat")

;    (make-dataset :n 24 :path "data/ratio/knap_0.0001.inst.dat")
;    (make-dataset :n 24 :path "data/ratio/knap_0.001.inst.dat")
;   (make-dataset :n 24 :path "data/ratio/knap_0.01.inst.dat")
    (make-dataset :n 24 :path "data/ratio/knap_0.1.inst.dat")
    (make-dataset :n 24 :path "data/ratio/knap_0.2.inst.dat")
    (make-dataset :n 24 :path "data/ratio/knap_0.4.inst.dat")
    (make-dataset :n 24 :path "data/ratio/knap_0.3.inst.dat")
    (make-dataset :n 24 :path "data/ratio/knap_0.5.inst.dat")
    (make-dataset :n 24 :path "data/ratio/knap_0.6.inst.dat")
    (make-dataset :n 24 :path "data/ratio/knap_0.7.inst.dat")
    (make-dataset :n 24 :path "data/ratio/knap_0.8.inst.dat")
    (make-dataset :n 24 :path "data/ratio/knap_0.9.inst.dat");
;
    (make-dataset :n 24 :path "data/granul/knap_0.0001.inst.dat")
    (make-dataset :n 24 :path "data/granul/knap_0.001.inst.dat")
    (make-dataset :n 24 :path "data/granul/knap_0.01.inst.dat")
    (make-dataset :n 24 :path "data/granul/knap_0.1.inst.dat")
    (make-dataset :n 24 :path "data/granul/knap_1.inst.dat")
    (make-dataset :n 24 :path "data/granul/knap_2.inst.dat")
    (make-dataset :n 24 :path "data/granul/knap_4.inst.dat")
    (make-dataset :n 24 :path "data/granul/knap_8.inst.dat")
    (make-dataset :n 24 :path "data/granul/knap_16.inst.dat")
    
)))
  


(defun run-experiments ()
  (let ((datasets (get-datasets))
	(res nil))
    (format t "n;path;B&Btime;GreedyTime;DynTime;FPTAS1Time;FPTAS2Time;FPTAS3Time;FPTAS4Time;DynWeightErr;DynPriceErr;GreedyErr;fptas1Err;fptas2err;fptas3err;fptas4err;~%")

   (dolist (ds datasets)
      (setf res (experiment ds))
      (format t "~D;~S;~5,5F;~5,5F;~5,5F;~5,5F;~5,5F;~5,5F;~5,5F;~5,5F;~5,5F;~5,5F;~5,5F;~5,5F;~5,5F;~5,5F;~%" 
	      (dataset-n ds)
	      (dataset-path ds)
	      (method-result-duration (exp-result-bb res))
	      (method-result-duration (exp-result-greedy res))
	      (method-result-duration (exp-result-dyn res))
	      (method-result-duration (exp-result-fptas1 res))
	      (method-result-duration (exp-result-fptas2 res))
	      (method-result-duration (exp-result-fptas3 res))
	      (method-result-duration (exp-result-fptas4 res))
;	      (method-result-step-counter (exp-result-bb res))
;	      (method-result-step-counter (exp-result-dyn res))
;	      (method-result-step-counter (exp-result-fptas1 res))
;	      (method-result-step-counter (exp-result-fptas2 res))
;	      (method-result-step-counter (exp-result-fptas3 res))
;	      (method-result-step-counter (exp-result-fptas4 res))
	      (method-result-rel-error (exp-result-dyn res))
	      (method-result-rel-error (exp-result-price res))
	      (method-result-rel-error (exp-result-greedy res))
	      (method-result-rel-error (exp-result-fptas1 res))
	      (method-result-rel-error (exp-result-fptas2 res))
	      (method-result-rel-error (exp-result-fptas3 res))
	      (method-result-rel-error (exp-result-fptas4 res)))
)))

(defstruct method-result (step-counter 0) (duration 0) (rel-error 0))

(defstruct exp-result
  (bb (make-method-result))
  (price (make-method-result))
  (dyn (make-method-result))
  (fptas1 (make-method-result))
  (fptas2 (make-method-result))
  (fptas3 (make-method-result))
  (fptas4 (make-method-result))
  (fptas5 (make-method-result))
  (fptas6 (make-method-result))
  (greedy (make-method-result))
  )


(defun proc-result (result result-agg)
  (incf (method-result-step-counter result-agg) (result-step-counter result))
  (incf (method-result-duration result-agg) (- (result-end-time result) (result-start-time result)))
  (incf (method-result-rel-error result-agg) (result-price result)))

(defun skipb (knap)
  (let 
      ((l (knapsack-items knap))
       (i (knapsack-id knap)))
    (or  (= 
	  (caar l) 
	  (cdar l) 
	  (caadr l) 
	  (cdadr l))
	 (= i 9557))))
  
;(defstruct result id solution price step-counter start-time end-time)  
(defun experiment (dataset)
  (let
      ((instances (load-knapsack (dataset-path dataset)))
       (exp-res (make-exp-result))
       (tmp-res nil)
       (inst-count 0)
       (before-time 0)
       (after-time 0))
    (dolist (knap instances)
;     (print "processing instance")
 ;     (print knap)
      (when (not (skipb knap))
	(incf inst-count))
;	(print "not skipped")
      ;--------------------- Greedy  ---------------------------------
      (setf before-time (get-internal-real-time))
      (setf tmp-res (greedy knap))
      (setf after-time (get-internal-real-time))
      (setf (result-start-time tmp-res) before-time)
      (setf (result-end-time tmp-res) after-time)
      (proc-result tmp-res (exp-result-greedy exp-res))

      ;--------------------- Branch  ---------------------------------
      (setf before-time (get-internal-real-time))
      (setf tmp-res (bb-algorithm knap))
      (setf after-time (get-internal-real-time))
      (setf (result-start-time tmp-res) before-time)
      (setf (result-end-time tmp-res) after-time)
      (proc-result tmp-res (exp-result-bb exp-res))
      ;---------------------   Price ---------------------------------
      (setf before-time (get-internal-real-time))
      (setf tmp-res (dyn-price-alg knap))
      (setf after-time (get-internal-real-time))
      (setf (result-start-time tmp-res) before-time)
      (setf (result-end-time tmp-res) after-time)
      (proc-result tmp-res (exp-result-price exp-res))
      ;------------------------ Weight ------------------------------
      (setf before-time (get-internal-real-time))
      (setf tmp-res (dyn-td-algorithm knap))
      (setf after-time (get-internal-real-time))
      (setf (result-start-time tmp-res) before-time)
      (setf (result-end-time tmp-res) after-time)
      (proc-result tmp-res (exp-result-dyn exp-res))
      ;------------------------- FTAS1  -----------------------------
      (setf before-time (get-internal-real-time))
      (setf tmp-res (dyn-price-alg (approximate-knapsack-prices knap 1) knap))
      (setf after-time (get-internal-real-time))
      (setf (result-start-time tmp-res) before-time)
      (setf (result-end-time tmp-res) after-time)
      (proc-result tmp-res (exp-result-fptas1 exp-res))
      ;---------------------------FPTAS 2---------------------------
      (setf before-time (get-internal-real-time))
      (setf tmp-res (dyn-price-alg (approximate-knapsack-prices knap 2) knap))
      (setf after-time (get-internal-real-time))
      (setf (result-start-time tmp-res) before-time)
      (setf (result-end-time tmp-res) after-time)
      (proc-result tmp-res (exp-result-fptas2 exp-res))
      ;------------------------- FPTAS 3 -----------------------------
      (setf before-time (get-internal-real-time))
      (setf tmp-res (dyn-price-alg (approximate-knapsack-prices knap 3) knap))
      (setf after-time (get-internal-real-time))
      (setf (result-start-time tmp-res) before-time)
      (setf (result-end-time tmp-res) after-time)
      (proc-result tmp-res (exp-result-fptas3 exp-res))
      ;-------------------------FPTAS 4 -----------------------------
      (setf before-time (get-internal-real-time))
      (setf tmp-res (dyn-price-alg (approximate-knapsack-prices knap 4) knap))
      (setf after-time (get-internal-real-time))
      (setf (result-start-time tmp-res) before-time)
      (setf (result-end-time tmp-res) after-time)
      (proc-result tmp-res (exp-result-fptas4 exp-res))
      ;(print (approximate-knapsack-prices knap 10))
      ;(print tmp-res)
      ;(break)


    ;; prumerovani
    (let ((solution-price (method-result-rel-error (exp-result-dyn exp-res))))
      (post-proc-results inst-count (exp-result-bb exp-res) solution-price)
      (post-proc-results inst-count (exp-result-dyn exp-res) solution-price)
      (post-proc-results inst-count (exp-result-price exp-res) solution-price)

      (post-proc-results inst-count (exp-result-greedy exp-res) solution-price)
      (post-proc-results inst-count (exp-result-fptas1 exp-res) solution-price)
      (post-proc-results inst-count (exp-result-fptas2 exp-res) solution-price)
      (post-proc-results inst-count (exp-result-fptas3 exp-res) solution-price)
      (post-proc-results inst-count (exp-result-fptas4 exp-res) solution-price)
      (post-proc-results inst-count (exp-result-fptas5 exp-res) solution-price)
      (post-proc-results inst-count (exp-result-fptas6 exp-res) solution-price))
    )
    exp-res
))

(defun post-proc-results (inst-count result-agg solution-price)
  
  (setf (method-result-step-counter result-agg) 
	(/ (method-result-step-counter result-agg) inst-count))

  (setf (method-result-duration result-agg) 
	(/ (method-result-duration result-agg) inst-count))

  (setf (method-result-rel-error result-agg)
	(/ 
	 (- solution-price (method-result-rel-error result-agg)) 
	 solution-price)))





(defstruct pos-item
  (price-weight-ratio)
  (weight 0 :type integer)
  (pos 0 :type integer)
  )


(defun greedy (knapsack)
  (let* ((items (knapsack-items knapsack ))
	 (pos-items (map 'list
			 #'(lambda (x y) (make-pos-item 
					  :price-weight-ratio (/ (cdr x) (car x))
					  :pos y
					  :weight (car x)))
			 items
			 (range 0 (length items))))
	 (sorted-items (sort pos-items #'> :key #'pos-item-price-weight-ratio))
	 (total-weight 0)
	 (result (make-sequence 'bit-vector (length items) :initial-element 0))
	 )	 
    (loop for item in sorted-items
       do (when  (> (knapsack-capacity knapsack) (+ (pos-item-weight item) total-weight))
	    (setf total-weight (+ total-weight (pos-item-weight item)))
	    (setf (bit result (pos-item-pos item)) 1)))
  (make-result :id (knapsack-id knapsack) :solution (array-to-list result) :price (get-price (array-to-list result) knapsack)  :step-counter 0)
  )
)

(defun range (start end)
  (loop for i from start below end collect i))



(defun array-to-list (array)
  (let* ((dimensions (array-dimensions array))
         (depth      (1- (length dimensions)))
         (indices    (make-list (1+ depth) :initial-element 0)))
    (labels ((recurse (n)
               (loop for j below (nth n dimensions)
                     do (setf (nth n indices) j)
                     collect (if (= n depth)
                                 (apply #'aref array indices)
                               (recurse (1+ n))))))
      (recurse 0))))


(defun copy-items (items) 
  (let ((res nil))
    (dolist (item items) 
      (setf res (cons (cons (car item) (cdr item)) res)))
    (reverse res)
    )

)

(defun approximate-knapsack-prices (knap ratio) 
  (let ((knap-approx (make-knapsack 
		      :id (knapsack-id knap)
		      :n (knapsack-n knap)
		      :capacity (knapsack-capacity knap)
		      :items (copy-items (knapsack-items knap)))

))
    (mapcar #'(lambda (x) 
		(setf (cdr x) 
		      (if (> (ash (cdr x) (- ratio)) 0) 
			  (ash (cdr x) (- ratio)) 
			  1))) 
	    (knapsack-items knap-approx)) 
    
    knap-approx
    )
)

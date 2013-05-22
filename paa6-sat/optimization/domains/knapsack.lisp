;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: knapsack.lisp

;;;; The Knapsack Problem

(defstructure (knapsack-problem (:include problem) 
			(:constructor create-knapsack-problem))
  (id 0) ;PAA speciality
  (items nil)
  (capacity 0)
  )

(defun make-knapsack-problem (&key (id 0) (items nil)
			      (capacity 0))
  "Constructor for Knapsack problem. Initial state is empty knapack with every item excluded."
  (create-knapsack-problem 
   :initial-state (make-sequence 'bit-vector (length items) :initial-element 0) 
   :id id     ; PAA
   :items items
   :dimension (length items)
   :capacity capacity)
  )

(defmethod objective-fn ((problem knapsack-problem) (state bit-vector)) 
  (floor (* (fitness-fn problem state) (penalty-fn problem state))))

(defmethod fitness-fn ((problem knapsack-problem) (state bit-vector))
  (apply #'+ (map 'list 
		  #'(lambda (x y) 
		      (* x (item-price y)))
		  state (knapsack-problem-items problem))))

(defmethod penalty-fn ((problem knapsack-problem) (state bit-vector)) 
  "Compute penalty koeficient in range <0,1>"
  (let ((items (knapsack-problem-items problem))
	(capacity (knapsack-problem-capacity problem)))
    (- 1 
       (/ 
	(abs (- (compute-weight state items) capacity))
	(penalty-delta capacity items)))))

(defmethod penalty-delta ((capacity integer) (items list))
  (max capacity 
       (abs (- 
	     (compute-weight 
	      (make-array (length items) :element-type 'bit :initial-element 1) 
	      items)
	     capacity))))

(defstruct pos-item
  (price-weight-ratio)
  (weight 0 :type integer)
  (pos 0 :type integer)
  )

; repair
(defmethod greedy-repair ((problem knapsack-problem) (state bit-vector))
  (let* ((items (knapsack-problem-items problem ))
	 (pos-items (map 'list
			 #'(lambda (x y) (make-pos-item 
					  :price-weight-ratio (/ (item-price x) (item-weight x))
					  :pos y
					  :weight (item-weight x)))
			 items
			 (range 0 (length items))))
	 (sorted-items (sort pos-items #'> :key #'pos-item-price-weight-ratio))
	 (total-weight 0)
	 (new-state (init-state items))
	 )
    (loop for item in sorted-items
       do (when (and (= 1 (bit state (pos-item-pos item))) 
		     (> (knapsack-problem-capacity problem) (+ (pos-item-weight item) total-weight)))
	    (setf total-weight (+ total-weight (pos-item-weight item)))
	    (setf (bit new-state (pos-item-pos item)) 1)))
    new-state))



(defmethod successors ((problem knapsack-problem) (state bit-vector))
  "Return a list of (action . state) pairs.  Actions are just the name of
  the city to go to.  You can only go to a city you haven't visited yet,
  unless you've visited them all, in which case you can only go back home."
  (let ((masks nil))
    (dotimes (mask-bit-idx (1- (length state))) 
      (let ((mask-vector (make-sequence 'bit-vector (length state) :initial-element 0)))
	(setf (bit mask-vector mask-bit-idx) 1)
	(push mask-vector masks)))
    (map 'list 
	 #'(lambda (item mask) (cons item (bit-xor state mask)))
	 (knapsack-problem-items problem) masks)))


(defmethod init-random-state ((problem knapsack-problem))
  (make-random-bit-vector (knapsack-problem-dimension problem)))

(defun compute-weight (config items)
  (apply #'+ (map 'list #'(lambda (x y) (* x (item-weight y))) config items)))

(defun compute-price (config items)
  (apply #'+ (map 'list #'(lambda (x y) (* x (item-price y))) config items)))


(defstruct item
  (weight 0 :type integer)
  (price  0 :type integer)
  )


(defun range (start end)
  (loop for i from start below end collect i))


(defun init-state (items)
  (make-array (length items) :element-type 'bit :initial-element 0))

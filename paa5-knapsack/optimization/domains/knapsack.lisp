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
  "TBD: penalty"
  (apply #'+ (map 'list 
		  #'(lambda (x y) 
		      (* x (item-price y))) 
		  state (knapsack-problem-items problem))))


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

;(defun get-weight (config knap)
;  (apply #'+ (mapcar #'(lambda (x y) (* x (car y))) config (knapsack-items knap))))


(defstruct item
  (weight 0 :type integer)
  (price  0 :type integer)
  )


;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: knapsack.lisp

;;;; The Knapsack Problem

(defstructure (sat-problem (:include problem) 
			(:constructor create-sat-problem))
  (id 0) 
  (clauses nil)
  (literals nil)
;PAA speciality
;  (items nil)
;  (capacity 0)
  )

(defstruct literal
  (index 0)
  (weight 0)
  (neg nil)
)

;(defun count-satisfied-clauses (genome))

(defstruct clause
  (literals)
)

(defun make-sat-problem (&key (id 0) (clauses nil) (literals nil))
  "Constructor for SAT problem. Initial state is empty knapack with every item excluded."
  (create-sat-problem 
   :initial-state (make-sequence 'bit-vector (length literals) :initial-element 0)
   :id id     
   :dimension (length literals)
   :literals literals
   :clauses clauses)
  )

(defmethod objective-fn ((problem sat-problem) (state bit-vector)) 
  (floor (* (fitness-fn problem state) (penalty-fn problem state))))

(defmethod fitness-fn ((problem sat-problem) (state bit-vector))
  (map 'list (lambda (literal bit)
	       (* (literal-weight literal) bit)) state)
)

(defmethod penalty-fn ((problem sat-problem) (state bit-vector)) 
  "Compute penalty koeficient in range <0,1>"
  (/ (length (get-satisfied-clauses problem state)) (length (sat-problem-clauses problem)))
)

(defmethod is-satisfied ((clause clause) (state bit-vector))
  (is-satisfied-iter (clause-literals clause) state)
)

(defun is-satisfied-iter (literals state)
  (if (not literals)
      T
      (let ((lit (car literals)))
	(if (or 
	     (and (= (bit state (literal-index lit)) 1) (literal-neg lit))
	     (and (= (bit state (literal-index lit)) 0) (not (literal-neg lit))))
	    NIL
	    (is-satisfied-iter (cdr literals) state)))))


(defmethod get-satisfied-clauses ((problem sat-problem) (state bit-vector))
  (let ((satisfied-clauses nil))
    (loop for clause in (sat-problem-clauses problem)
       do (if (is-satisfied clause state)
	      (setf satisfied-clauses (cons clause satisfied-clauses))))
  satisfied-clauses))



(defmethod successors ((problem sat-problem) (state bit-vector))
  "Return a list of (action . state) pairs.  Actions are just the name of
  the city to go to.  You can only go to a city you haven't visited yet,
  unless you've visited them all, in which case you can only go back home."
  nil)

(defmethod init-random-state ((problem sat-problem))
  (make-random-bit-vector (sat-problem-dimension problem)))


(defun range (start end)
  (loop for i from start below end collect i))


(defun init-state (literals)
  (make-array (length literals) :element-type 'bit :initial-element 0))

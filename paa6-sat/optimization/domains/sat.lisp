(declaim (optimize (speed 3) (safety 0) (debug 0)))

;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: knapsack.lisp

;;;; The Knapsack Problem

(defstructure (sat-problem (:include problem) 
			(:constructor create-sat-problem))
  (id 0) 
  (clauses nil)
  (literals nil)
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
  (floor (* (fitness-fn problem state) (penalty-fn-multi problem state))))

(defmethod fitness-fn ((problem sat-problem) (state bit-vector))
  (apply #'+
	 (map 'list 
	      (lambda (literal bit) (* (literal-weight literal) bit))
	      (sat-problem-literals problem) 
	      state)))

(defmethod penalty-fn-multi ((problem sat-problem) (state bit-vector)) 
  "Compute penalty koeficient in range <0,1>"
  (/ (length (get-satisfied-clauses problem state)) (length (sat-problem-clauses problem)))
)

(defmethod penalty-fn-add ((problem sat-problem) (state bit-vector)) 
  "Compute penalty koeficient in range <0,1>"
  (let ((result 0))
    (dolist (cl (get-unsatisfied-clauses problem state))
      (incf result (get-largest-literal-weight cl)))
    result
)

(defmethod get-largest-literal-weight ((clause clause))
  (let ((res 0))
    (dolist (lit (clause-literals clause))
      (if (> (literal-weight lit) res)
	  (setf res (literal-weight lit))))
    res))


(defmethod is-satisfied ((clause clause) (state bit-vector))
  (is-satisfied-iter (clause-literals clause) state)
)

(defun is-satisfied-iter (literals state)
  (if (not literals)
      NIL
      (let ((lit (car literals)))
	(if (or 
	     (and (= (bit state (literal-index lit)) 0) (literal-neg lit))
	     (and (= (bit state (literal-index lit)) 1) (not (literal-neg lit))))
	    T
	    (is-satisfied-iter (cdr literals) state)))))


(defmethod get-satisfied-clauses ((problem sat-problem) (state bit-vector))
  (let ((satisfied-clauses nil))
    (loop for clause in (sat-problem-clauses problem)
       do (if (is-satisfied clause state)
		(setf satisfied-clauses (cons clause satisfied-clauses))))
       satisfied-clauses))

(defmethod get-unsatisfied-clauses ((problem sat-problem) (state bit-vector))
  (let ((unsatisfied-clauses nil))
    (loop for clause in (sat-problem-clauses problem)
       do (if (not (is-satisfied clause state))
		(setf unsatisfied-clauses (cons clause unsatisfied-clauses))))
       unsatisfied-clauses))



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


(defstruct bf-state
  (state)
  (order)
)

(defmethod brute-force ((sat sat-problem))
;; takes knapsack and returns bestConfiguration
  (let* ((stack nil)
	(var-num (length (sat-problem-literals sat)))
	(init-0 (make-bf-state :state (make-sequence 'bit-vector var-num :initial-element 0) :order 0))
	(init-1 (make-bf-state :state (make-sequence 'bit-vector var-num :initial-element 0) :order 1))
	)
    (setf (bit (bf-state-state init-1) 0) 1)
    (push init-0 stack)
;    (push init-1 stack)
    (bf-optimize sat stack 0 nil)
    ))
  

(defun is-solution (state problem)
;  (print "sat claus")
;  (print (get-satisfied-clauses problem #*1000))
;  (break)
  (= (length (sat-problem-clauses problem)) (length (get-satisfied-clauses problem state))))

(defun bf-optimize (sat stack solution res-iter)
;  (print "STACK:")
 ; (print stack)
  (let* ((stack-top (pop stack)))

;    (print "Top of stack contains: ")
;    (prin1 stack-top)
    (if (not stack-top) 
	(progn
	  (print res-iter)
	  solution)
	(progn
	  (if (= (bf-state-order stack-top) (sat-problem-dimension sat))
	      (progn 
		(if (is-solution (bf-state-state stack-top) sat)
					; solution found
		    (let ((res (fitness-fn sat (bf-state-state stack-top))))
		      (if (>= res solution)
			  (progn
			    (setf solution res)
			    (setf res-iter stack-top)
			    ))))))
	  (let ((child-states (get-child-states stack-top)))		    
	    (if (car child-states)
		(push (car child-states) stack))
	    (if (cadr child-states)
		(push (cadr child-states) stack))
	    (bf-optimize sat stack solution res-iter)
	    )))))



(defun get-child-states (bf-state)
  (let ((order (bf-state-order bf-state)))
    (if (> order (- (length (bf-state-state bf-state)) 1))
	nil
	(let
	    ((state-0 (copy-seq (bf-state-state bf-state)))
	     (state-1 (copy-seq (bf-state-state bf-state))))
	  (setf (bit state-0  order) 0)
	  (setf (bit state-1  order) 1)
	  (list (make-bf-state :state state-0 :order (1+ order) )
		(make-bf-state :state state-1 :order (1+ order) ))))))
		

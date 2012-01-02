;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: genetic.lisp


;;;; Genetic Algorithm

(defun genetic-algorithm (goal pop-size chromosome-size tries)
  (let ((pool (create-initial-pool pop-size chromosome-size))
        (fitness))
    (loop for i from 1 to tries
       do (setf fitness (pool-fitness pool goal))
       if (there-is-a-winner pool fitness) return it
       do (display-turn pool fitness i)
       do (setf pool (re-populate pool fitness))
       finally (return (find-best-chromosome pool fitness)))))



(defun genetic-algorithm (problem 
			  &optional 
			  (pop-init-fn #'init-random-population) 
			  (pop-size 100)
			  (stopping-criterion #'num-of-generations))
  "Some useful comment."
  (let (
	(population  (make-population pop-init-fn pop-size)))

    (loop until (reached-stopping-criterion? stopping-criterion population) 
       do (print "Loop start")
       do (setf population ())
       do (print "update result set"))
    nil))


(defun breed (population selection-fn mutation-fn crossover-fn)
  "Hardcoded repopulation method. Should be configurable in the future"
  (apply #'concatenate 'list 
	 (loop for iter from 1 to (/ (population-size old) 2)
	    collect (mutate (cross crossover-fn 
				   (select selection-fn population) 
				   (select selection-fn population))))))

(defun re-populate (old-pop new-pop scaling-scheme-fn)
  "Hardcoded repopulation method. Repop method shoud be selectable and configurable in the future."

)

;;;; Aux
(defun reached-stopping-criterion? (criterion-fn population)
  (funcall criterion-fn population))

(defun select (selection-fn pop)
  "Takes population. Returns single chromosome selected by selection-fn."
  (funcall selection-fn pop)
)

(defun cross (crossover-fn x-chromosome y-chromosome)
  "Takes two chromosomes and applies onthem crossover function. Returns cons of their offspring."
  (funcall crossover-fn x-chromosome y-chromosome)
)

(defun mutate (mutation-fn chromosomes)
  "maps mutatio function on given chromosome list."
  (map mutation-fn chromosomes)
)

(defun repopulate old-population new-population elitism)

(defun rescale scaling scheme)

;;;; Stop conditions

(defun make-generation-age-condition-fn (limit-age) 
  #'(lambda (population) (= limit-age (population-age population))))


;;;; Populations
(defstructure (population (:constructor create-population))
  (pool nil :type list)
  (age 0)
  (size 0)
)


(defun make-population (init-fn size problem)
  (let (
	(pop (create-population
	      :pool (make-array size :element-type genome) 
	      :number 0
	      :size 0
	      )))
    (funcall init-fn pop size problem)))


;;;; Initializers
(defun init-random-population (population size problem)
  ;; populate with random states
  (dotimes (idx population-size)
    (setf (aref population-pool idx) (make-random-genome problem)))

;;;; Selections


;;;; Fitness Scaling



;;;; End condition

;;;; Genome

(defstructure (genome (:include node)
		      (:constructor create-genome))
    (age 0)
)

(defstructure (genome-bit-vector (:include genome (state nil :type state-bit-vector))
				 (:constructor create-genome-bit-vector))
)

;; Genome Constructors 

(defmethod make-genome ((state state-bit-vector))
  (create-genome-bit-vector :state state))

;;(defmethod make-genome (state state-tree)
;;  (create-genome-tree :state state))


;;;; Genome Initializers

(defmethod make-random-genome ((problem problem))
  (make-genome (make-random-state problem)))


;;(defmethod make-random-genome-solution ((problem problem)) 
;;  (make-genome (make-random-state problem)))


;;;; Operations

;; opc for bit vectors
(defmethod one-point-crossover ((x-genome genome-bit-vector) (y-genome genome-bit-vector) )
  (nil)

)


; slot fitness



;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: genetic.lisp


;;;; Genetic Algorithm


(defstruct ga-config 
  (population-size 100 :type integer)

  (population-init-fn #'init-random-population :type function)
  (stopping-criterion-fn (make-generation-age-condition-fn 1000) :type function)
  (scaling-scheme-fn (make-null-scaling-scheme) :type function)

  (selection-fn (make-roulette-selection) :type function)
  (crossover-fn (make-one-point-crossover) :type function)
  (mutation-fn (make-bit-flip-mutation 0.5) :type function)
  )


(defun genetic-algorithm (problem 
			  &optional 
			  (config (make-ga-config)))
  "Some useful comment."

  (let* (
	 (pop-init-fn (ga-config-population-init-fn config))
	 (pop-size (ga-config-population-size config))
	 (stop-crit-fn (ga-config-stopping-criterion-fn config))
	 (scale-fn (ga-config-scaling-scheme-fn config))
	 (population  (make-population pop-init-fn pop-size problem)))

    (loop until (reached-stopping-criterion? stop-crit-fn population) 
       do (print "Loop start")
       do (setf (population-pool population) (repopulate population (breed population config)))
       do (incf (population-age population))
       do (rescale scale-fn population)
       do (print "update result set"))
    nil))


(defun breed (population config)
  "Hardcoded breed method. Should be configurable in the future"
  (let (
	(mutation-fn (ga-config-mutation-fn config))
	(crossover-fn (ga-config-crossover-fn config))
	(selection-fn (ga-config-selection-fn config)))
    (apply #'concatenate 'list 
	   (loop for iter from 1 to (/ (population-size population) 2)
	      collect (mutate mutation-fn (cross crossover-fn 
						 (select selection-fn population) 
						 (select selection-fn population)))))))


(defun rescale (population scaling-scheme-fn)
  "Updates fitness for every chromosome in population."
  (funcall scaling-scheme-fn population))

(defun repopulate (population genome-pool)
  "Hardcoded repopulation method. Repop method shoud be selectable and configurable in the future."
  genome-pool
)

;;;; Aux
(defun reached-stopping-criterion? (criterion-fn population)
  (funcall criterion-fn population))

(defun select (selection-fn population)
  "Takes population. Returns single chromosome selected by selection-fn."
  (funcall selection-fn population)
)

(defun cross (crossover-fn x-genome y-genome)
  "Takes two genomes and applies onthem crossover function. Returns list of their offspring."
  (funcall crossover-fn x-genome y-genome)
)

(defun mutate (mutation-fn genomes)
  "maps mutation function on given chromosome list."
  (map 'list mutation-fn genomes)
)


;;;; Stop conditions

(defun make-generation-age-condition-fn (limit-age) 
  (lambda (population) (= limit-age (population-age population))))


;;;; Rescale schemes
(defun make-null-scaling-scheme ()
  (lambda (population genome-pool) 
    (print population)
    (print genome-pool)))



;;;; Selections

;; Roulette selection

;; Constructor
(defun make-roulette-selection ()
  "This constructor is here just for convention preservation."
  #'roulette-selection)

;; Implementation
(defun roulette-selection (population)
  (let* (
	(fitness-sum (apply #'+ (map 'list #'genome-fitness (population-pool population))))
	(random-roll (random fitness-sum)))

    (defun inner-loop (genomes-list roll-remainder)
      (if (< roll-remainder (genome-fitness (first genomes-list)))
	  (first genomes-list)
	  (inner-loop (rest genomes-list) 
		      (- roll-remainder (genome-fitness (first genomes-list))))))
    (inner-loop (population-pool population) random-roll)))

;;;; Mutations; uses side effects!!

(defmethod make-bit-flip-mutation ((mutation-rate float))
  "Constructor method that crates lambda holdin closure which immediately call appropriate method with closured value. Oh god my english really sucks!"
  (if (or (< mutation-rate 0.0) (> mutation-rate 1.0))
      (error "Mutation rate must be from interval [0.0 - 1.0]")
      (lambda (genome) (bit-flip-mutation mutation-rate genome))))



(defmethod bit-flip-mutation ((mutation-rate float) (genome genome-bit-vector))
  "This method flips one bit if blah blah TBD doc here"
  (if (< mutation-rate (random 1.0))
      genome     ; mutation-rate is lower than random roll so we don;t mutate anything
      (flip-bit (genome-state genome) (random (length (genome-state genome))))))



;;;; Crossovers; doesnt use side effects

(defun make-one-point-crossover ()
  #'one-point-crossover)

(defmethod one-point-crossover ((x-genome genome-bit-vector) (y-genome genome-bit-vector))
  "NOT effective!!!"
  (let (
	(mask-vec (fill 
		   (make-sequence 'bit-vector 
				  (length (genome-state x-genome))
				  :initial-element 0)
		   1
		   :end (random (length (genome-state x-genome)))))) ; random crosspoint

    (list (make-genome (bit-ior                               ;new x-genome
			(bit-and (genome-state y-genome) mask-vec)
			(bit-and (genome-state x-genome) (bit-not mask-vec)) 
			t))
	  (make-genome (bit-ior                               ;new y-genome
			(bit-and (genome-state x-genome) mask-vec)
			(bit-and (genome-state y-genome) (bit-not mask-vec)) 
			t)))))


;;;; Auxiliary functions
(defun flip-bit (vector index)
  "EVIL function! uses side effects!"
  (setf (bit vector index) 
	(if (= (bit vector index) 0)
	    1
	    0)))


;;;; Populations
(defstructure (population (:constructor create-population))
  (pool nil :type list)
  (age 0)
  (size 0)
)


(defun make-population (init-fn size problem)
  (create-population
   :pool (funcall init-fn size problem)
   :age 0
   :size 0
   ))
  


;;;; Initializers
(defun init-random-population-pool (size problem)
  ;; populate with random states
  (loop for iter from 1 to size
     collect (make-random-genome problem)))


;;;; Genome

(defstructure (genome (:include node)
		      (:constructor create-genome))
    (age 0)
)

(defstructure (genome-bit-vector (:include genome (state nil :type bit-vector))
				 (:constructor create-genome-bit-vector))
)

;; Genome Constructors 

(defmethod make-genome ((state bit-vector))
  (create-genome-bit-vector :state state))

;;(defmethod make-genome (state state-tree)
;;  (create-genome-tree :state state))


;;;; Genome Initializers

(defmethod make-random-genome ((problem problem))
  (make-genome (init-random-state problem)))


;;(defmethod make-random-genome-solution ((problem problem)) 
;;  (make-genome (make-random-state problem)))



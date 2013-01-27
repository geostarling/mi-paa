;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: genetic.lisp
(declaim (optimize (speed 2) (safety 3) (debug 3)))



;;;; Genetic Algorithm


(defstruct ga-config 
  (population-size 10 :type integer)

  (population-init-fn #'init-random-population :type function)
  (stopping-criterion-fn (make-generation-age-condition-fn 10) :type function)
  (scaling-scheme-fn (make-identity-scaling-scheme) :type function)
  (repopulation-fn (make-simple-repopulation) :type function)

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
	 (repopulation-fn (ga-config-repopulation-fn config))

	 (selection-fn (ga-config-selection-fn config))
	 (crossover-fn (ga-config-crossover-fn config))
	 (mutation-fn (ga-config-mutation-fn config))

	 (population (make-population pop-init-fn pop-size problem)))

    (loop until (reached-stopping-criterion? stop-crit-fn population) 
       do (break)
       do (print "Loop start")
       do (rescale scale-fn (population-pool population))
       do (print-genome-pool (population-pool population))
       do (setf (population-pool population)
		(repopulate repopulation-fn 
			    population 
			    (breed population selection-fn crossover-fn mutation-fn)
			    problem))
       do (incf (population-age population))
       do (print "update result set"))
    
(sort 
		    (copy-list (population-pool population))
		    #'>
		    :key #'genome-fitness))
    nil)
  
  )


(defun get-result (population)
  (sort 
   (copy-list (population-pool population))
   #'>
   :key #'genome-fitness))


;;;; Genome

(defstructure (genome (:include node)
		      (:constructor create-genome))
    (scaled-fitness 0)
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




;;;; Operations

(defun breed (population selection-fn crossover-fn mutation-fn)
  "Hardcoded breed method. Should be configurable in the future"
  (apply #'concatenate 'list 
	 (loop for iter from 1 to (round (/ (population-size population) 2))
	    collect (mutate mutation-fn (cross crossover-fn 
					       (select selection-fn population) 
					       (select selection-fn population))))))


(defun rescale (scaling-scheme-fn genome-pool)
  "Updates fitness for every genome in population."
  (funcall scaling-scheme-fn genome-pool))

(defun repopulate (repopulate-fn population genome-pool problem)
  "This function doesnt changes given population structure. Just returns list of new generation genomes that must be set to population structure."
  (recalculate-fitness genome-pool problem)
  (funcall repopulate-fn population genome-pool)
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

(defmethod recalculate-fitness ((genome-pool list) (problem problem))
  (map 'list 
       #'(lambda (gen) (recalculate-fitness gen problem))
       genome-pool))

(defmethod recalculate-fitness ((gen genome) (problem problem))
  (setf (genome-fitness gen) (objective-fn problem (genome-state gen)))
  gen)

;;;; Stop conditions

(defun make-generation-age-condition-fn (limit-age) 
  (lambda (population) (= limit-age (population-age population))))


;;;; Rescale schemes
(defun make-identity-scaling-scheme ()
  "Takes result of problem's objective function as genome's fitness. I.E. does no rescaling."
  #'identity-scaling-scheme)

(defmethod identity-scaling-scheme ((genome-pool list))
  (dolist (gen genome-pool)
    (setf (genome-scaled-fitness gen) (genome-fitness gen)))
  genome-pool)


;;;; Repopulations

(defun make-simple-repopulation (&optional (elite-count 1))
; TBD check elite-count boundary
  (lambda (population genome-pool)
    (let
	((old-pool (sort 
		    (copy-list (population-pool population))
		    #'>
		    :key #'genome-fitness))
	 (new-pool (sort (copy-list genome-pool) #'> :key #'genome-fitness))
	 (result-pool nil))
      (dotimes (idx elite-count)  ;; add elite into result
	(push (first old-pool) result-pool)
	(setf old-pool (rest old-pool)))
     ; (print "old")
     ; (print old-pool)
      ;(print "result")
     ; (print result-pool)
    ;  (print "new-pool")
   ;   (print new-pool)
  ;    (print genome-pool)
 ;     (break)
      (loop for genome in new-pool
	 until (= (length result-pool) (length genome-pool))
;	 do (print "len res")
;	 do (print (length result-pool))
;	 do (print "len gen pool")
;	 do (print (length genome-pool))
	 do (push genome result-pool))
      result-pool)))

(defun make-steady-state-repopulation ()
  nil 
)

;;;; Selections

;; Roulette selection

;; Constructor
(defun make-roulette-selection ()
  "This constructor is here just for convention preservation."
  #'roulette-selection)

;; Implementation
(defun roulette-selection (population)
  ;; tady je problem v pripade kdy maji vschny genomy fitness 0 tak random vrati chybu
  (let* ((fitness-sum (apply #'+ (map 'list #'genome-scaled-fitness (population-pool population))))
	(random-roll (random fitness-sum)))

    (defun inner-loop (genomes-list roll-remainder)
      (if (< roll-remainder (genome-scaled-fitness (first genomes-list)))
	  (first genomes-list)
	  (inner-loop (rest genomes-list) 
		      (- roll-remainder (genome-scaled-fitness (first genomes-list))))))
    (inner-loop (population-pool population) random-roll)))

;;;; Mutations; uses side effects!!

(defmethod make-bit-flip-mutation ((mutation-rate float))
  "Constructor method that crates lambda holdin closure which immediately call appropriate method with closured value."
  (if (or (< mutation-rate 0.0) (> mutation-rate 1.0))
      (error "Mutation rate must be from interval [0.0 - 1.0]")
      (lambda (genome) (bit-flip-mutation mutation-rate genome))))



(defmethod bit-flip-mutation ((mutation-rate float) (genome genome-bit-vector))
  "This method flips one bit if blah blah TBD doc here"
  (if (< mutation-rate (random 1.0))
      genome     ; mutation-rate is lower than random roll so we don;t mutate anything
;tady chyba
      (mutate-bit genome (random (length (genome-state genome))))))


;;;; Auxiliary functions
(defmethod mutate-bit ((genome genome-bit-vector) index)
  "EVIL function! uses side effects!"
  (setf (bit (genome-state genome) index) 
	(if (= (bit (genome-state genome) index) 0)
	    1
	    0))
  genome)


;;;; Crossovers; dont use side effects

(defun make-one-point-crossover ()
  #'one-point-crossover)

(defmethod one-point-crossover ((x-genome genome-bit-vector) (y-genome genome-bit-vector))
  "NOT effective!!!"
  (let (
	(mask-vec (fill 
		   (make-sequence 'bit-vector 
				  (length (genome-state x-genome))
				  :initial-element 0)
		   1 :end (random (length (genome-state x-genome)))))) ; random crosspoint

    (list (make-genome (bit-ior                               ;new x-genome
			(bit-and (genome-state y-genome) mask-vec)
			(bit-and (genome-state x-genome) (bit-not mask-vec)) 
			t))
	  (make-genome (bit-ior                               ;new y-genome
			(bit-and (genome-state x-genome) mask-vec)
			(bit-and (genome-state y-genome) (bit-not mask-vec)) 
			t)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Populations
(defstructure (population (:constructor create-population))
  (pool nil :type list)
  (age 0)
  (size 0)
)


(defun make-population (init-fn size problem)
  (create-population
   :pool (recalculate-fitness (funcall init-fn size problem) problem)
   :age 0
   :size size
   )
)
  


;;;; Initializers
(defun init-random-population (size problem)
  ;; populate with random states
  (loop for iter from 1 to size
     collect (make-random-genome problem)))

;;;; Printers
;(defmethod print-structure ((node node) stream)
;  (format stream "#<NODE f(~D) = g(~D) + h(~D) state:~A>" (node-f-cost node)
;          (node-g-cost node) (node-h-cost node) (node-state node)))

(defmethod print-genome-pool (pool)
  (dolist (gen pool)
    (print-structure gen t)))

(defmethod print-structure ((gen genome) stream)
  (format stream "#<GENOME fitness:~D  state:~A>~%" (genome-fitness gen) (genome-state gen)))




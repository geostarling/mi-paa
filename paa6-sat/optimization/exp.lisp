

(defstruct exp-result id var-num cl-num history time sat)

(defun run-experiment ()
  (let ((data-list (list
		    (list *dimacs-1-list* "datasat/results-dimacs/penalty/rate-5-dimacs-1.csv" )
		    (list *dimacs-2-list* "datasat/results-dimacs/penalty/rate-5-dimacs-2.csv" )
		    (list *dimacs-3-list* "datasat/results-dimacs/penalty/rate-5-dimacs-3.csv" )
		    (list *dimacs-4-list* "datasat/results-dimacs/penalty/rate-5-dimacs-4.csv" )
		    (list *dimacs-5-list* "datasat/results-dimacs/penalty/rate-5-dimacs-5.csv" )
		    (list *dimacs-6-list* "datasat/results-dimacs/penalty/rate-5-dimacs-6.csv" )
;		    (list *dimacs-7-list* "datasat/results-dimacs/mutation-rate/mut-rate-001-dimacs-7.csv" )

)))
  (dolist (ins data-list)
    (experiment (car ins) (cadr ins ))
    )
))


(defun experiment (data-list out-file)
  (prin1 "out-file:" )
  (print out-file)
  (let ((dataset (load-sat-all data-list))
	(results nil)
	(config (make-ga-config :population-size 100
				:stopping-criterion-fn (make-generation-age-condition-fn 500)
;				:scaling-scheme-fn (make-linear-scaling-scheme 0.7)
				:crossover-fn (make-one-point-crossover 0.99)
				:repopulation-fn (make-simple-repopulation 3 1)
				:mutation-fn (make-bit-flip-mutation 0.15) )))
    (with-open-file (str out-file
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (dolist (instance dataset)

	(let ((result nil)
	      (before nil)
	      (after nil))
	      (setf before (get-internal-real-time))
	      (print (sat-problem-id instance))
	      (setf result (genetic-algorithm instance config))
	      (setf after (get-internal-real-time))
	      (setf results  (cons 
			      (make-exp-result :id (sat-problem-id instance) :var-num (sat-problem-dimension instance) :cl-num (length (sat-problem-clauses instance)) :history (result-history result) :time (- after before) :sat (result-satisfied result))
			      results))))
      (mapcar 
       (lambda (exp-res)
	 (format str "~5,3F;" (exp-result-id exp-res))
	 (format str "~5,3F;" (exp-result-var-num exp-res))
	 (format str "~5,3F;" (exp-result-cl-num exp-res))
	 (format str "~5,3F;" (exp-result-sat exp-res))

	 (mapcar (lambda (res)
		   (format str "~5,3F;" res))
		 (exp-result-history exp-res))

	 (format str "~%"))
       results))))


(defstruct bf-res var-num clauses-num solution)

(defun bf-experiment ()
  (let ((iter 0)
	(dataset (load-sat-all 
		  (reverse *dimacs-4-list*



))))
    (with-open-file (str "/home/watanabe/mi-paa/paa6-sat/datasat/solutions-dimacs-4.csv"
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (dolist (instance dataset)
	(let ((result (brute-force instance)))
	      (print result)
	      (format str "~5,3F;" (1+ iter))
	      (incf iter)
	      (setf iter (mod iter 5))
	      (format str "~5,3F;" (sat-problem-dimension instance))
	      (format str "~5,3F;" (length (sat-problem-clauses instance)))
	      (format str "~5,3F;" result)
	      (format str "~%"))
	))))

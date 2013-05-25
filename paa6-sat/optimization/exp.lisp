

(defstruct exp-result id history time)


"-rw-rw-r-- 1 watanabe users  3683 Dec 31 20:51 knap_10.inst.dat
-rw-rw-r-- 1 watanabe users  5188 Dec 31 20:51 knap_15.inst.dat
-rw-rw-r-- 1 watanabe users  6779 Dec 31 20:51 knap_20.inst.dat
-rw-rw-r-- 1 watanabe users  7450 Dec 31 20:51 knap_22.inst.dat
-rw-rw-r-- 1 watanabe users  8426 Dec 31 20:51 knap_25.inst.dat
-rw-rw-r-- 1 watanabe users  9029 Dec 31 20:51 knap_27.inst.dat
-rw-rw-r-- 1 watanabe users 10045 Dec 31 20:51 knap_30.inst.dat
-rw-rw-r-- 1 watanabe users 10671 Dec 31 20:51 knap_32.inst.dat
-rw-rw-r-- 1 watanabe users 11638 Dec 31 20:51 knap_35.inst.dat
-rw-rw-r-- 1 watanabe users 12282 Dec 31 20:51 knap_37.inst.dat
-rw-rw-r-- 1 watanabe users 13265 Dec 31 20:51 knap_40.inst.dat
-rw-rw-r-- 1 watanabe users  1833 Dec 31 20:51 knap_4.inst.dat
"

(defun experiment ()
  (let ((dataset (load-sat-all (list "datasat/uf20-09.cnf")))
	(results nil)
	(config (make-ga-config :population-size 35 
				:stopping-criterion-fn (make-generation-age-condition-fn 100) 
				:repopulation-fn (make-simple-repopulation 1)
				:mutation-fn (make-bit-flip-mutation 0.999) )))
    (with-open-file (str "/tmp/out.csv"
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (dolist (instance dataset)

	(let ((result nil)
	      (before nil)
	      (after nil))
	      (setf before (get-internal-real-time))
	      (setf result (genetic-algorithm  instance config))
	      (setf after (get-internal-real-time))

	      (setf results  (cons 
			      (make-exp-result :id (knapsack-problem-id instance) :history (result-history result) :time (- after before))
			  results))))
      (mapcar 
       (lambda (exp-res)
	 (format str "~5,3F;" (exp-result-id exp-res))
	 (format str "~5,3F;" (exp-result-time exp-res))
	 (mapcar (lambda (res)
		   (format str "~5,3F;" res))
		 (exp-result-history exp-res))
	 (format str "~%"))
       results))))




(defun bf-experiment ()
  (let ((dataset (load-sat-all (list "datasat/random_ksat.dimacs"))))


    (with-open-file (str "/tmp/out.csv"
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (dolist (instance dataset)

	(let ((result nil)
	      (before nil)
	      (after nil))
	      (setf before (get-internal-real-time))
	      (setf result (brute-force instance))
	      (setf after (get-internal-real-time))
	      (print result))))))

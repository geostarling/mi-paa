;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: tsp.lisp

;;;; The Travelling Salesperson Problem (TSP)

;;; Find a tour: a path that visits every city exactly once, and returns to
;;; the starting city.  The shorter the total distance, the better.  This
;;; builds on the map data structure defined in route-finding.lisp.  It
;;; assumes that the map is a complete graph: there is a path from every city
;;; to every other city.
;;;
;;; Note: the TSP is NP complete in the general case, but there are some good
;;; algorithms for finding approximate solutions, particularly when the
;;; triangle inequality is satisfied (that the path from A->C is always
;;; shorter than A->B->C).  Many of these algorithms are based on the idea of
;;; building a minimum spanning tree, converting it into a tour, and perhaps
;;; modifying it.  We don't go into that here (because we are more interested
;;; in hooking up to the general search procedures than in special-purpose
;;; algorithms), but note that our tsp-h heuristic function is a relaxed
;;; version of a minimum spanning tree.

(defstructure (knapsack (:include problem) 
			   (:constructor create-knapsack-problem))
  (items nil)
  (capacity nil))

(defun make-knapsack-problem (&key (items nil)
			      (capacity 0))
  "Constructor for Knapsack problem. Initial state is empty knapack with every item excluded."
  (create-knapsack-problem 
   :initial-state (make-knap :excluded items
			     :included nil)
   :items items
   :capacity capacity))

(defmethod edge-cost ((problem tsp-problem) node action state)
  (declare (ignore action))
  (road-distance (find-city (tsp-city-name (node-state node))
			    (tsp-problem-map problem))
		 (tsp-city-name state)))

(defun get-price (config knap)
  (apply #'+ (mapcar #'(lambda (x y) (* x (cdr y))) config (knapsack-items knap))))

(defun get-weight (config knap)
  (apply #'+ (mapcar #'(lambda (x y) (* x (car y))) config (knapsack-items knap))))


(defmethod h-cost ((problem tsp-problem) state)
  "A lower bound on the cost is the distance to ???"
  (let ((to-visit (tsp-to-visit state))
	(map (tsp-problem-map problem)))
    (+ (nearest-neighbor-distance (tsp-city-name state) to-visit map)
       (nearest-neighbor-distance (tsp-start state) to-visit map)
       (path-lower-bound to-visit map))))

(defmethod successors ((problem knapsack-problem) state)
  "Return a list of (action . state) pairs.  Actions are just the name of
  the city to go to.  You can only go to a city you haven't visited yet,
  unless you've visited them all, in which case you can only go back home."
  (let ((result nil))

    (for each item in (knap-included state) do
      (push (cons item 
		  (make-knap
		   :included (remove item (knap-included state))
		   :excluded (cons item (knap-excluded state)))) 
	    result))

    (for each item in (knap-excluded state) do
      (push (cons item 
		  (make-knap
		   :included (cons item (knap-included state))
		   :excluded (remove item (knap-excluded state))))
	    result))
    result))

(defmethod goal-test ((problem tsp-problem) state)
  "The goal is to leave no unvisited cities and get back to start."
  (and (null (tsp-to-visit state))
       (eql (tsp-city-name state) 
	    (tsp-city-name (problem-initial-state problem)))))

(defstruct item
  (weight 0 :type integer)
  (price  0 :type integer)
  )

(defstruct (knap (:type bit-vector))
  "A state for Knapsack problem."
  (state nil)
  )

;;;; Auxiliary Functions

(defun nearest-neighbor-distance (name candidate-names map)
  "Find among the CANDIDATE-NAMES of cities, the one that is closest to
  city NAME, and return the distance to it."
  (if (null candidate-names)
      0
    (let ((city (find-city name map))
	  (distance infinity))
       (for each other-name in candidate-names do
	    (unless (eq other-name name)
	      (setf distance (min distance (road-distance city other-name)))))
       distance)))

(defun path-lower-bound (city-names map)
  "Find a lower bound for a path through these cities."
  ;; Each city must be connected to a next one, for n-1 links for n cities.
  ;; A lower bound is the sum of the shortest links for each city but first.
  (let ((sum 0))
   (for each name in (rest city-names) do
	(incf sum (nearest-neighbor-distance name city-names map)))
   sum))

(defun random-tsp-map (&key (n-cities 6))
  (random-route-map :n-cities n-cities :min-roads (- n-cities 1)
			       :max-roads (- n-cities 1)))

(defun check-tsp-map? (map)
  (for each city in map do
       (when (/= (length (city-neighbors city)) (- (length map) 1))
	 (error "This map can't be used for a travelling salesperson problem ~
                because ~A is not connected to every other city."
		(city-name city)))))

(defun tsp-city-name (tsp-state)
  "The current city: the last one visited."
  ;; We store the cities visited in reverse order, so take the first one
  (first (tsp-visited tsp-state)))

(defun tsp-start (tsp-state)
  (last1 (tsp-visited tsp-state)))


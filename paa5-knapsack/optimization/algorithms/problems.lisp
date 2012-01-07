;; -*- Mode: Lisp -*-

;;;; Defining Problems

(defstructure problem
  "A problem is defined by the initial state, and the type of problem it is.
  We will be defining subtypes of PROBLEM later on.  For bookkeeping, we
  count the number of nodes expanded.  Note that the three other fields from
  the book's definition [p 60] have become generic functions; see below."
  (initial-state (required)) ; A state in the domain
  (dimension (required))
  (num-expanded 0)           ; Number of nodes expanded in search for solution.
  )

;;; When we define a new subtype of problem, we need to define a SUCCESSORS
;;; method. We may need to define methods for GOAL-TEST, H-COST, and
;;; EDGE-COST, but they have default methods which may be appropriate.

(defmethod successors ((problem problem) state)
  "Return an alist of (action . state) pairs, reachable from this state."
  (declare-ignore state)
  (error "You need to define a SUCCESSORS method for ~A" problem))

(defmethod objective-fn ((problem problem) state) 
  ""
  (declare (ignore state))
  0)

;;;; Manipulating Nodes

(defstructure node
  "Node for generic search.  A node contains a state, a domain-specific
  representation of a point in the search space. "
  (state (required))        ; a state in the domain
  (parents nil :type list)  ; list containing parent nodes of this node
  (action nil)              ; the domain action leading to state
  (fitness 0)
;  (successors nil)          ; list of sucessor nodes
;  (unexpanded nil)          ; successors not yet examined (SMA* only)
  (depth 0)                 ; depth of node in tree (root = 0)
  (cost 0)                  ; 
  (expanded? nil)           ; any successors examined?
;  (completed? nil)          ; all successors examined? (SMA* only)
  )

(defstruct state
  (repr (required))         ; internal representation of state
  )

;(defstruct (state-bit-vector (:include state (repr nil :type bit-vector)))
;  )



(defun expand (node problem)
  "Generate a list of all the nodes that can be reached from a node."
  ;; Note the problem's successor-fn returns a list of (action . state) pairs.
  ;; This function turns each of these into a node.
  ;; If a node has already been expanded for some reason, then return no nodes,
  ;; unless we are using an iterative algorithm.
  (unless (node-expanded? node)
    (setf (node-expanded? node) t)
    (incf (problem-num-expanded problem))
    (let ((nodes nil))
      (for each (action . state) in (successors problem (node-state node)) do
	     (push
	      (make-node 
	       :parents (list node) :action action :state state
	       :depth (1+ (node-depth node)) 
	       :cost (node-cost node))
	      nodes))
      nodes)))

;;;; Manipulating Solutions

;;; Solutions are represented just by the node at the end of the path.  The
;;; function SOLUTION-ACTIONS returns a list of actions that get there.  It
;;; would be problematic to represent solutions directly by this list of
;;; actions, because then we couldn't tell a solution with no actions from a
;;; failure to find a solution. 

(defun solution-actions (node &optional (actions-so-far nil))
  "Return a list of actions that will lead to the node's state."
  (cond ((null node) actions-so-far)
	((null (node-parents node)) actions-so-far)
	(t (solution-actions (node-parents node)
			     (cons (node-action node) actions-so-far)))))

(defun solution-nodes (node &optional (nodes-so-far nil))
  "Return a list of the nodes along the path to the solution."
  (cond ((null node) nodes-so-far)
	(t (solution-nodes (node-parents node)
			   (cons node nodes-so-far)))))

(defun solve (problem &optional algorithm)
  "Print a list of actions that will solve the problem (if possible).
  Return the node that solves the problem, or nil."
  (setf (problem-num-expanded problem) 0)
  (let ((node (funcall algorithm problem)))
    (print-solution problem node)
    node))

(defun print-solution (problem node)
  "Print a table of the actions and states leading up to a solution."
  (if node
      (format t "~&Action ~20T State~%====== ~20T =====~%")
    (format t "~&No solution found.~&"))
  (for each n in (solution-nodes node) do
       (format t "~&~A ~20T ~A~%"
	       (or (node-action n) "") (node-state n)))
  (format t "====== ~20T =====~%Total of ~D node~:P expanded."
	  (problem-num-expanded problem))
  node)

;;;; Auxiliary functions

(defun make-random-bit-vector (dimension)
  (let ((vector (make-sequence 'bit-vector dimension :initial-element 0)))
    (dotimes (idx (length vector))
      (setf (bit vector idx) (random 2)))
    vector))


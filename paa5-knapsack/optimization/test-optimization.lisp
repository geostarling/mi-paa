;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: search/test-optimization.lisp

;;;; Test Cases for Search

(deftest genetic
  "Test the code for Solving Problems by Searching"
  "Start with a trivial version of the missionaries and cannibals puzzle."

  ((setq dataset (load-all)))
  ((print (car dataset)))
  ((solve (car dataset) 'genetic-algorithm) *)
  

  )



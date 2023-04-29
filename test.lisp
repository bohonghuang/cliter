(defpackage cliter.test
  (:use #:cl #:parachute)
  (:import-from #:alexandria #:rcurry)
  (:export #:suite))

(in-package #:cliter.test)

(define-test suite)

(define-test list-iterator :parent suite
  (let ((iter (cliter:list-iterator '(1 2 3))))
    (is-values (cliter:next iter) (= 1) (equal '(1 2 3)))
    (is-values (cliter:next iter) (= 2) (equal '(2 3)))
    (is-values (cliter:next iter) (= 3) (equal '(3)))
    (is-values (cliter:next iter) (eql nil) (eql nil)))
  (is equal '(1 2 3) (cliter:iterator-list (cliter:list-iterator '(1 2 3)))))

(define-test vector-iterator :parent suite
  (let ((iter (cliter:vector-iterator #(1 2 3))))
    (is-values (cliter:next iter) (= 1) (eql t))
    (is-values (cliter:next iter) (= 2) (eql t))
    (is-values (cliter:next iter) (= 3) (eql t))
    (is-values (cliter:next iter) (eql nil) (eql nil)))
  (is equalp #(1 2 3) (cliter:iterator-vector (cliter:vector-iterator #(1 2 3)))))

(define-test length-nth :parent suite
  (is = 2 (cliter:nth 1 (cliter:vector-iterator #(1 2 3))))
  (is = 3 (cliter:length (cliter:vector-iterator #(1 2 3)))))

(define-test map :parent suite
  (let ((iter1 (cliter:vector-iterator #(1 2 3)))
        (iter2 (cliter:list-iterator '(7 6 5 4))))
    (is equal '(8 8 8) (cliter:iterator-list (cliter:map #'+ iter1 iter2)))))

(define-test flatmap :parent suite
  (let ((iter1 (cliter:map #'cliter:list-iterator (cliter:vector-iterator #((1) (2) (3)))))
        (iter2 (cliter:map #'cliter:vector-iterator (cliter:list-iterator '(#(7) #(6) #(5) #(4))))))
    (is equal '(1 7 2 6 3 5) (cliter:iterator-list (cliter:flatmap #'cliter:append iter1 iter2)))))

(define-test remove :parent suite
  (is equal '(2 4 6) (cliter:iterator-list (cliter:remove-if #'oddp (cliter:vector-iterator #(1 2 3 4 5 6)))))
  (is equal '("2" "3" "4" "5" "6") (cliter:iterator-list (cliter:remove 1 (cliter:list-iterator '("1" "2" "3" "4" "5" "6")) :key #'parse-integer :test #'=)))
  (is equal '(1 3 5) (cliter:iterator-list (cliter:remove-if-not #'oddp (cliter:vector-iterator #(1 2 3 4 5 6))))))

(define-test append :parent suite
  (let ((iter (cliter:append (cliter:list-iterator '())
                             (cliter:list-iterator '(1 2 3))
                             (cliter:list-iterator '())
                             (cliter:list-iterator '(4 5 6))
                             (cliter:list-iterator '()))))
    (is equal '(1 2 3 4 5 6) (cliter:iterator-list iter))))

(define-test reduce :parent suite
  (is = 6 (cliter:reduce #'+ (cliter:list-iterator '(1 2 3))))
  (is = 14 (cliter:reduce #'+ (cliter:list-iterator '(1 2 3)) :initial-value 5 :key #'1+)))

(define-test count :parent suite
  (is = 3 (cliter:count-if #'oddp (cliter:vector-iterator #(1 2 3 4 5))))
  (is = 3 (cliter:count 1 (cliter:list-iterator '("1" "1" "1" "4" "5" "6")) :key #'parse-integer :test #'=))
  (is = 2 (cliter:count-if-not #'oddp (cliter:vector-iterator #(1 2 3 4 5)))))

(define-test position :parent suite
  (is = 0 (cliter:position-if #'oddp (cliter:vector-iterator #(1 2 3 4 5))))
  (is = 2 (cliter:position 3 (cliter:list-iterator '("1" "2" "3" "4" "5" "6")) :key #'parse-integer :test #'=))
  (is = 1 (cliter:position-if-not #'oddp (cliter:vector-iterator #(1 2 3 4 5)))))

(define-test find :parent suite
  (is = 1 (cliter:find-if #'oddp (cliter:vector-iterator #(1 2 3 4 5))))
  (is string= "3" (cliter:find 3 (cliter:list-iterator '("1" "2" "3" "4" "5" "6")) :key #'parse-integer :test #'=))
  (is = 2 (cliter:find-if-not #'oddp (cliter:vector-iterator #(1 2 3 4 5)))))

(define-test remove-duplicates :parent suite
  (is equal '(1 2 3) (cliter:iterator-list (cliter:remove-duplicates (cliter:vector-iterator #(1 1 2 2 3 3)))))
  (is equal '("1" "2" "3") (cliter:iterator-list (cliter:remove-duplicates (cliter:vector-iterator #("1" "1" "2" "2" "3" "3")) :test #'equal))))

(define-test subseq :parent suite
  (is equal '(2 3 4) (cliter:iterator-list (cliter:subseq (cliter:vector-iterator #(1 2 3 4 5)) 1 4)))
  (is equal '(3 4 5) (cliter:iterator-list (cliter:subseq (cliter:vector-iterator #(1 2 3 4 5)) 2)))
  (is equal '(1 2 3) (cliter:iterator-list (cliter:take (cliter:vector-iterator #(1 2 3 4 5)) 3)))
  (is equal '(3 4 5) (cliter:iterator-list (cliter:drop (cliter:vector-iterator #(1 2 3 4 5)) 2))))

(define-test drop-until-take-while :parent suite
  (is equal '(2 3 4) (cliter:iterator-list (cliter:drop-until-take-while (rcurry #'= 2) (rcurry #'<= 4) (cliter:vector-iterator #(1 2 3 4 5)))))
  (is equal '(2 3 4 5) (cliter:iterator-list (cliter:drop-until (rcurry #'= 2) (cliter:vector-iterator #(1 2 3 4 5)))))
  (is equal '(3 4 5) (cliter:iterator-list (cliter:drop-while (rcurry #'< 3) (cliter:vector-iterator #(1 2 3 4 5)))))
  (is equal '(1 2 3 4) (cliter:iterator-list (cliter:take-while (rcurry #'< 5) (cliter:vector-iterator #(1 2 3 4 5)))))
  (is equal '(1 2 3 4) (cliter:iterator-list (cliter:take-until (rcurry #'= 5) (cliter:vector-iterator #(1 2 3 4 5))))))

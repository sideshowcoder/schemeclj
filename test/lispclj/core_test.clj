(ns lispclj.core-test
  (:require [clojure.test :refer :all]
            [lispclj.core :refer :all]))

(deftest lclj-rep-test
  (testing "Basic arithmetic"
    (is (= 10 (lclj-rep "(begin (+ 5 5))"))))
  (testing "Define variable"
    (is (= 10 (lclj-rep "(begin (define x 5) (+ x x))"))))
  (testing "Test quote"
    (is (= 'test (lclj-rep "(begin (quote test))"))))
  (testing "Test if"
    (is (= 10 (lclj-rep "(begin (if (= 1 1) 10 1))"))))
  (testing "Test define and call procendure with variable shadowing"
    (is (= 10 (lclj-rep "(begin (define r 1) (define square-area (lambda (r) (* r r))) (+ r (square-area 3)))")))))

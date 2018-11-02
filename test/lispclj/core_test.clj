(ns lispclj.core-test
  (:require [clojure.test :refer :all]
            [lispclj.core :refer :all]))

(deftest tokenize-test
  (let [sample-program "(begin (define r 10) (* pi (* r r)))"]
    (testing "Tokenize sample program."
      (is (= (tokenize sample-program)
             '("(" "begin" "(" "define" "r" "10" ")" "(" "*" "pi" "(" "*" "r" "r" ")" ")" ")"))))))

(deftest read-tokens-test
  (let [sample-program-tokens '("(" "begin" "(" "define" "r" "10" ")" "(" "*" "pi" "(" "*" "r" "r" ")" ")" ")")]
    (testing "Create AST from tokens."
      (is (= (read-tokens sample-program-tokens)
             [["begin" ["define" "r" "10"] ["*" "pi" ["*" "r" "r"]]]])))))

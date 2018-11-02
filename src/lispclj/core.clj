(ns lispclj.core
  (:require [clojure.string :as s]))


(defn tokenize
  "Split a INPUT string in scheme compatible tokens."
  [input]
  (let [split-input (-> input
                        (s/replace #"\)" " ) ")
                        (s/replace #"\(" " ( ")
                        (s/split #" "))]
    (filter not-empty split-input)))

(defmacro safe
  [body]
  `(try ~body (catch Exception e# nil)))

(defn str->int [n] (safe (Integer/parseInt n)))

(defn str->double [n] (safe (Double/parseDouble n)))


(defn token->value
  "Parse atom to type."
  [t]
  (or (str->int t)
      (str->double t)
      (symbol t)))

(defn read-tokens
  [tokens]
  (loop [[t & ts] tokens
         ast []]
    (cond
      (= t "(") (let [{rts :tokens sub-ast :ast} (read-tokens ts)]
                  (recur rts (conj ast sub-ast)))
      (= t ")") {:ast ast :tokens ts}
      (not (nil? t)) (recur ts (conj ast (token->value t)))
      (nil? ts) ast
      :true (throw (RuntimeException. (str "Syntax error, unexpected token: " t))))))

(defn parse
  [input]
  (-> input
      (tokenize)
      (read-tokens)))

;; TODO define environment for functions and lookups of variables
;; TODO define eval taking in an environment and transforming it according to the ast

(parse "(begin (define r 10) (* pi (* r r)))")

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
      (read-tokens)
      first))

(defn identity-nil
  ([] nil)
  ([x] x))

(def lclj-base-env
  {:+ +
   :* *
   :> >
   :pi java.lang.Math/PI
   :begin identity-nil})

(defn if-token?
  [x]
  (= 'if (first x)))

(declare lclj-eval)

(defn if-branch
  [x env]
  (let [[_ test t-branch f-branch] x]
    (if (lclj-eval test env) t-branch f-branch)))

(defn define-token?
  [x]
  (= 'define (first x)))

(defn lclj-eval
  "Eval a expression X in the context of environment ENV, defaults to
  base-env."
  ([x] (lclj-eval x lclj-base-env))
  ([x env]
   (cond
     (symbol? x) {:result ((keyword x) env) :env env}
     (number? x) {:result x :env env}
     (if-token? x) (lclj-eval (if-branch x env) env)
     (define-token? x) (let [[_ var exp] x
                             {value :result env1 :env} (lclj-eval exp env)]
                         {:env (assoc env1 (keyword var) value)})
     :else (let [[proc-sym & raw-args] x
                 {env1 :env proc :result} (lclj-eval proc-sym env)
                 {env2 :current-env args :args} (reduce (fn [{current-env :current-env args :args} next-arg]
                                                             (let [{updated-env :env eval-arg :result} (lclj-eval next-arg current-env)]
                                                               {:current-env updated-env :args (if (nil? eval-arg) args (conj args eval-arg))}))
                                                           {:current-env env1 :args []}
                                                           raw-args)]
             {:env env2 :result (apply proc args)}))))

(parse "(begin (define r 10) (* pi (* r r)))")

(:result (lclj-eval (parse "(begin (+ 1 10))")))

(:result (lclj-eval (parse "(begin (define r 10))")))

(:result (lclj-eval (parse "(begin (define r 10) (* pi (* r r)))")))

;; (:result (lclj-eval (parse "(+ 1 10)")))
;; (lclj-eval (parse "(define r 10)"))

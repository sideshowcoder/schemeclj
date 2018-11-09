(ns lispclj.core
  (:require [clojure.string :as s]
            [clojure.core.match :refer [match]]))

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
   := =
   :pi java.lang.Math/PI
   :begin identity-nil})

(declare lclj-eval)

(defn if-branch
  [[_ test t-branch f-branch] env]
  (if (:result (lclj-eval test env)) t-branch f-branch))

(defn define-in-env
  [[_ var exp] env]
  (let [{value :result env1 :env} (lclj-eval exp env)]
    (assoc env1 (keyword var) value)))

(defn lclj-fn-call
  [[proc-sym & raw-args] env]
  (let [{env1 :env proc :result} (lclj-eval proc-sym env)
        {env2 :current-env args :args} (reduce (fn [{current-env :current-env args :args} next-arg]
                                                 (let [{updated-env :env eval-arg :result} (lclj-eval next-arg current-env)]
                                                   {:current-env updated-env :args (if (nil? eval-arg) args (conj args eval-arg))}))
                                               {:current-env env1 :args []}
                                               raw-args)]
    {:env env2 :result (apply proc args)}))

(defn lclj-fn
  [[_ params body] env]
  (let [keyword-params (map keyword params)]
    (fn [& args]
      (let [fn-env (into env (map vector keyword-params args))]
        (:result (lclj-eval body fn-env))))))

(defn lclj-eval
  "Eval a expression X in the context of environment ENV, defaults to
  base-env."
  ([x] (lclj-eval x lclj-base-env))
  ([x env]
   (match [x]
          [(_ :guard symbol?)] {:result ((keyword x) env) :env env}
          [(_ :guard number?)] {:result x :env env}
          [['if _ _ _]] (lclj-eval (if-branch x env) env)
          [['define _ _]] {:env (define-in-env x env)}
          [['quote _]] {:env env :result (-> x rest first)}
          [['lambda _ _]] {:env env :result (lclj-fn x env)}
          :else (lclj-fn-call x env))))

(defn lclj-rep
  "Read Eval Print, but no loop"
  [exp]
  (-> exp
      parse
      lclj-eval
      :result))

(lclj-rep "(begin (define r 10) (* pi (* r r)))")
(lclj-rep "(begin (if (= 1 0) 1 10))")
(lclj-rep "(begin (quote (the more the merrier the)))")
(lclj-rep "(begin (define r 10) (define circle-area (lambda (r) (* pi (* r r)))) (+ r (circle-area 3)))")

(ns clin.env
  (:require [clin.any :as any]
            [clin.parser :as parser]
            [clin.util :as util]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(defrecord ENV [code lines stack scope gscope])
(def dENV (->ENV any/dFN {} [] {} {}))

;UTIL

(defn s-get
  [{stack :stack} n]
  (let [l (count stack)
        i (->> n
               any/toINT
               bit-not
               (util/-i stack))]
    (println n i)
    (if (<= 0 i (dec l))
      (any/a-get stack i)
      (-> (str "stack len " l " < " n)
          Exception.
          throw))))

(defn push
  [{stack :stack, :as env} & xs]
  (->> xs
       (into stack)
       (assoc env :stack)))

(defn show
  [{stack :stack}]
  (->> stack
       (map any/show)
       (str/join " ")))

(defn set-code [env x] (assoc env :code (any/eFN x env)))

(defn arg
  [n {stack :stack, :as env} f]
  (let [l (count stack)]
    (if (> n l)
      (-> (str "stack len " l " < " n)
          Exception.
          throw)
      (let [[xs ys] (split-at (- l n) stack)]
        (-> env
            (assoc :stack xs)
            (cons ys)
            (#(apply f %)))))))

(defn mods
  [n env f]
  (arg n
       env
       (fn [env & xs]
         (->> xs
              (apply f)
              (apply push env)))))

(defn modx [n env f] (mods n env (fn [& xs] [(apply f xs)])))

;LIB

(defn PICK
  [env]
  (arg 1
       env
       (fn [env n]
         (->> n
              (s-get env)
              (push env)))))

(defn NIX
  [env]
  (arg 1
       env
       (fn [{stack :stack, :as env} n]
         (->> n
              (any/a-rem stack)
              (assoc env :stack)))))

(defn DUP
  [env]
  (-> env
      (push 0)
      PICK))

(defn OVER
  [env]
  (-> env
      (push 1)
      PICK))

(defn POP [env] (arg 1 env #(%)))

(defn NIP [env] (modx 2 env #(%2)))

(defn SWAP
  [env]
  (mods 2
        env
        #(-> [%2 %])))

(defn NEG [env] (modx 1 env #(any/numx - %)))

(defn ADD [env] (modx 2 env #(any/numx + % %2)))

(def cmds
  {"pick" PICK,
   "nix" NIX,
   "dup" DUP,
   "over" OVER,
   "pop" POP,
   "nip" NIP,
   "swap" SWAP,
   "_" NEG,
   "+" ADD})

(defn cmd
  [s env]
  (if-let [f (get cmds s)]
    (f env)
    (throw (.Exception (str "cmd " s " not found")))))

;RUN

(defn step
  [env t]
  (match t
    (_ :guard any/CMD?) (cmd (:x t) env)
    :else (push env t)))

(defn exec
  [{code :code, :as env}]
  (println (show env))
  (match (.-x code)
    ([] :seq) env
    ([t & ts] :seq) (-> env
                        (step t)
                        (set-code ts)
                        recur)))

(defn run
  [s]
  (->> s
       parser/parse
       (set-code dENV)
       exec))

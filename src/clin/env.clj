(ns clin.env
  (:require [clin.any :as any]
            [clin.parser :as parser]
            [clin.util :as util]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(defrecord ENV [code stack lines gscope])
(def dENV (->ENV any/dFN [] {} {}))

;;; HELPERS

(defn s-get
  [{stack :stack} n]
  (let [l (count stack)
        i (->> n
               any/toINT
               bit-not
               (util/-i stack))]
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
  (let [l (count stack)
        i (- l n)]
    (if (neg? i)
      (-> (str "stack len " l " < " n)
          Exception.
          throw)
      (-> env
          (assoc :stack (subvec stack 0 i))
          (cons (subvec stack i))
          (->> (apply f))))))

(defn mods
  [n env f]
  (arg n
       env
       (fn [env & xs]
         (->> xs
              (apply f)
              (apply push env)))))

(defn modx [n env f] (mods n env (fn [& xs] [(apply f xs)])))

(defn numx [n env f] (modx n env #(apply any/numx f %&)))

(declare cmds)

(defn cmd
  [env s]
  (if-let [f (get cmds s)]
    (f env)
    (-> (str "cmd " s " not found")
        Exception.
        throw)))

(defn step
  [env t]
  (match t
    (_ :guard any/CMD?) (cmd env (:x t))
    :else (push env t)))

(defn exec
  [{code :code, :as env}]
  (println (show env))
  (match (.-x code)
    ([t & ts] :seq) (-> env
                        (step t)
                        (set-code ts)
                        recur)
    :else env))

(defn f-eval-e
  [env f]
  (if (any/FN? f)
    (-> env
        (assoc :code f)
        exec
        :stack
        (->> (assoc env :stack)))
    (recur env (any/eFN f env))))

(defn f-eval
  [{code :code, :as env} f]
  (if (any/FN? f)
    (if (empty? code) (assoc env :code f) (f-eval-e env f))
    (recur env (any/eFN f env))))

(defn run
  [s]
  (->> s
       parser/parse
       (set-code dENV)
       (trampoline exec)))

;;; LIB

(defn EVAL [env] (arg 1 env f-eval))

(defn EVALQ
  [env]
  (arg 1
       env
       #(-> %
            (f-eval-e %2)
            (s-get 0)
            (->> (push %)))))

(defn toSEQ [env] (modx 1 env #(any/toSEQ %)))

(defn toFN [env] (modx 1 env #(any/eFN % env)))

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

(defn DUP [env] (push env (s-get env 0)))

(defn OVER [env] (push env (s-get env 1)))

(defn POP [env] (arg 1 env (fn [x _] x)))

(defn NIP [env] (modx 2 env #(%2)))

(defn SWAP
  [env]
  (mods 2
        env
        #(-> [%2 %])))

(defn NEG [env] (numx 1 env -))

(defn ADD [env] (numx 2 env +))

(defn SUB [env] (numx 2 env -))

(defn MUL [env] (numx 2 env *))

(defn DIV [env] (numx 2 env /))

(defn MOD [env] (numx 2 env mod))

(def cmds
  {"#" EVAL,
   "Q" EVALQ,
   ">Q" toSEQ,
   ">F" toFN,
   "pick" PICK,
   "nix" NIX,
   "dup" DUP,
   "over" OVER,
   "pop" POP,
   "nip" NIP,
   "swap" SWAP,
   "_" NEG,
   "+" ADD,
   "-" SUB,
   "*" MUL,
   "/" DIV,
   "%" MOD})

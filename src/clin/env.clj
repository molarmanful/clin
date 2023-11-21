(ns clin.env
  (:require [clin.any :as any]
            [clin.parser :as parser]
            [clin.util :as util]
            [clojure.string :as str]
            [clojure.core.match :refer [match]])
  (:import [java.util.concurrent ConcurrentHashMap]))


(defrecord ENV [code stack lines gscope arr])
(def dENV (->ENV any/dFN [] (ConcurrentHashMap.) (ConcurrentHashMap.) ()))

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

(defn get-line
  [{:keys [code lines]} n]
  (println lines)
  (.get lines [(.-f code) n]))

(defn set-line
  [{:keys [code lines], :as env} n s f]
  (.put lines [(.-f code) n] [s f])
  env)

(defn fn-line
  [env n]
  (match (get-line env n)
    [s f] (set-line env n s (or f (any/lFN s env n)))
    :else env))

(defn load-line
  [env n]
  (fn-line env n)
  (let [[_ f] (get-line env n)] (if f (assoc env :code f) env)))

(defn init-lines
  [env ls]
  (->> ls
       (map-indexed #(-> [% %2]))
       (run! (fn [[i x]] (set-line env i x nil))))
  env)

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
  [{code :code, stack :stack, :as env}]
  (println (any/show stack))
  (match (.-x code)
    ([t & ts] :seq) (-> env
                        (set-code ts)
                        (step t)
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

(defn f-eval-s
  [env xs f]
  (-> env
      (assoc :stack xs)
      (f-eval-e f)
      :stack))

(defn f-eval-a1
  [env xs f]
  (-> env
      (f-eval-s xs f)
      (any/a-get -1)))

(defn f-eval-a2
  [env xs f]
  (let [s (f-eval-s env xs f)] [(s-get s -2) (s-get s -1)]))

(defn run
  [s]
  (-> dENV
      (init-lines (str/split-lines s))
      (load-line 0)
      (as-> $ (trampoline exec $))))

;;; LIB

(defn FORM [env] (modx 1 env any/show))

(defn EVAL [env] (arg 1 env f-eval))

(defn EVALq
  [env]
  (arg 1
       env
       #(-> %
            (f-eval-e %2)
            (s-get 0)
            (->> (push %)))))

(defn EVALl [env])

(defn EVALlrel [env])

(defn EVALlhere [env])

(defn EVALlnext [env])

(defn EVALlprev [env])

(defn toSEQ [env] (modx 1 env any/toSEQ))

(defn toFN [env] (modx 1 env #(any/eFN % env)))

(defn toARR [env] (modx 1 env any/toARR))

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

(defn DUPS [{stack :stack, :as env}] (push env stack))

(defn POP [env] (arg 1 env (fn [x _] x)))

(defn NIP [env] (modx 2 env #(%2)))

(defn CLR [env] (assoc env :stack []))

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

(defn WRAP [env] (modx 1 env vector))

(defn PAIR [env] (modx 2 env vector))

(defn WRAP* [{stack :stack, :as env}] (assoc env :stack [stack]))

(defn FNa
  [{code :code, :as env}]
  (let [[xs ys n] (any/lambda-loop (.-x code))
        [cs c] (if (and (<= n 0) (not-empty ys))
                 [(drop-last ys) (last ys)]
                 [ys (any/->CMD ")")])]
    (-> env
        (set-code xs)
        (push (any/eFN cs env))
        (step c))))

(defn ARRa
  [{:keys [arr stack], :as env}]
  (-> env
      (assoc :arr (cons stack arr))
      CLR))

(defn ARRb
  [{:keys [arr stack], :as env}]
  (if (empty? arr)
    env
    (-> env
        (assoc :stack (first arr)
               :arr (rest arr))
        (push stack))))

(defn MAP
  [env]
  (modx 2
        env
        (fn [a b] (any/vecz (fn [f] (any/a-map #(f-eval-a1 env [%] f) a)) b))))

(defn DOT [{code :code, :as env}] (if (empty? code) (EVALlnext env) ()))

(def cmds
  {"form" FORM,
   "#" EVAL,
   "Q" EVALq,
   "@@" EVALl,
   "@~" EVALlrel,
   "@" EVALlhere,
   ";" EVALlnext,
   ";;" EVALlprev,
   ">Q" toSEQ,
   ">F" toFN,
   ">A" toARR,
   "pick" PICK,
   "nix" NIX,
   "dup" DUP,
   "over" OVER,
   "dups" DUPS,
   "pop" POP,
   "nip" NIP,
   "clr" CLR,
   "swap" SWAP,
   "_" NEG,
   "+" ADD,
   "-" SUB,
   "*" MUL,
   "/" DIV,
   "%" MOD,
   ",," WRAP,
   "," PAIR,
   ",`" WRAP*,
   "(" FNa,
   ")" identity,
   "[" ARRa,
   "]" ARRb,
   "map" MAP,
   "." DOT})

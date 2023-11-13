(ns clin.parser
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require any))

(defrecord Parser [xs x t])
(def dParser (->Parser (lazy-seq []) "" ::UN))

(defn addc [{:keys [_ x _], :as p} c] (assoc p :x (str x c)))

(defn clean
  [{:keys [xs x t]}]
  (assoc dParser
    :xs (concat xs
                (match t
                  ::ESC [(any/wSTR (str x \\))]
                  ::STR [(any/wSTR x)]
                  ::CMD [(if (every? (fn [c] (contains? "([{}])" c)) x)
                           (map (comp any/wSTR str) x)
                           (any/wCMD x))]
                  ::DEC (match x
                          "." [(any/wCMD ".")]
                          (\. :<< last) [(any/wCMD butlast x)]
                          :else [(any/wNUM x)])
                  ::NUM [(any/wNUM x)]
                  :else []))))

(defn pcmd
  [{:keys [_ _ t], :as p} c]
  (match t
    ::CMD (addc p c)
    :else (assoc (addc (clean p) c) :t ::CMD)))

(defn pnum
  [{:keys [_ _ t], :as p} c]
  (match t
    (:or ::DEC ::NUM) (addc p c)
    :else (assoc (clean p) :t ::NUM)))

(defn pdot
  [{:keys [_ x t], :as p}]
  (assoc (addc (match t
                 ::NUM p
                 :else (clean p))
               (str x \.))
    :t ::DEC))

(defn pstr
  [p c]
  (match c
    \\ (assoc p :t ::ESC)
    \" (clean p)
    :else (addc p c)))

(defn pesc
  [p c]
  (assoc (addc p
               (match c
                 \" "\""
                 :else (str \\ c)))
    :t ::STR))

(defn choice
  [{:keys [_ _ t], :as p} c]
  (match t
    ::ESC (pesc p c)
    ::STR (pstr p c)
    :else (match c
            \" (assoc (clean p) :t ::STR)
            \. (pdot p)
            (_ :guard Character/isDigit) (pnum p c)
            (_ :guard Character/isWhitespace) (clean p)
            :else (pcmd p c))))

(defn parse-line [s] (reduce choice dParser s))

(defn parse [s] (parse-line (first (str/split-lines s))))

(parse "1234\"mafo\"asdf2+.3.xyz")

(ns clin.parser
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clin.any :as any]))

(defrecord Parser [xs x t])
(def dParser (->Parser (lazy-seq []) "" ::UN))

(defn addc [{x :x, :as p} c] (assoc p :x (str x c)))

(defn clean
  [{:keys [xs x t]}]
  (->> (match t
         ::ESC [(str x \\)]
         ::STR [x]
         ::CMD [(if (every? #(str/includes? "([{}])" (str %)) x)
                  (map str x)
                  (any/->CMD x))]
         ::DEC (match x
                 "." [(any/->CMD ".")]
                 (\. :<< last) [(any/->CMD (butlast x))]
                 :else [x])
         ::NUM [(any/toNUM x)]
         :else [])
       (lazy-cat xs)
       (assoc dParser :xs)))

(defn pcmd
  [{t :t, :as p} c]
  (match t
    ::CMD (addc p c)
    :else (-> p
              clean
              (addc c)
              (assoc :t ::CMD))))

(defn pnum
  [{t :t, :as p} c]
  (match t
    (:or ::DEC ::NUM) (addc p c)
    :else (-> p
              clean
              (addc c)
              (assoc :t ::NUM))))

(defn pdot
  [{t :t, :as p}]
  (as-> p $
    (match t
      ::NUM $
      :else (clean $))
    (addc $ \.)
    (assoc $ :t ::DEC)))

(defn pstr
  [p c]
  (match c
    \\ (assoc p :t ::ESC)
    \" (clean p)
    :else (addc p c)))

(defn pesc
  [p c]
  (-> p
      (addc (match c
              \" "\""
              :else (str \\ c)))
      (assoc :t ::STR)))

(defn choice
  [{t :t, :as p} c]
  (match t
    ::ESC (pesc p c)
    ::STR (pstr p c)
    :else (match c
            \" (assoc (clean p) :t ::STR)
            \. (pdot p)
            (_ :guard Character/isDigit) (pnum p c)
            (_ :guard Character/isWhitespace) (clean p)
            :else (pcmd p c))))

(defn parse-line
  [s]
  (->> s
       (reduce choice dParser)
       clean
       .xs))

(defn parse
  [s]
  (-> s
      str/split-lines
      first
      parse-line))

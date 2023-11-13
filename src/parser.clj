(ns parser
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [any]))

(def ^:private Pt #{:UN :STR :NUM :CMD :ESC :DEC})

(defrecord Parser [xs x t])
(def dParser (->Parser (lazy-seq []) "" :UN))

(defn pcmd [p c])

(defn pnum [p c])

(defn pdot [p])

(defn pstr [p c])

(defn pesc [p c])

(defn clean
  [{:keys [xs x t]}]
  (assoc dParser
    :xs (concat xs
                (match t
                  :ESC [(any/wSTR (conj x \\))]
                  :STR [(any/wSTR x)]
                  :CMD [(if (every? (fn [c] (contains? "([{}])" c)) x)
                          (map (comp any/wSTR str) x)
                          (any/wCMD x))]
                  :DEC (match x
                         "." [(any/wCMD ".")]
                         (\. :<< last) [(any/wCMD butlast x)]
                         :else [(any/wNUM x)])
                  :NUM [(any/wNUM x)]
                  :else []))))

(defn choice
  [{:keys [_ _ t], :as p} c]
  (match t
    :ESC (pesc p c)
    :STR (pstr p c)
    :else (match c
            \" (assoc (clean p) :t :STR)
            \. (pdot p)
            (_ :guard Character/isDigit) (pnum p c)
            (_ :guard Character/isWhitespace) (clean p)
            :else (pcmd p c))))

(defn parse-line [s] (reduce choice dParser s))

(defn parse [s] (parse-line (first (str/split-lines s))))

(parse "1234\"mafo\"asdf2+.3.xyz")

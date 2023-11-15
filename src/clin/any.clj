(ns clin.any
  (:require [clojure.string :as str]
            [clin.util :as util]
            [clojure.core.match :refer [match]]))

;CUSTOM

(def ARR clojure.lang.PersistentVector)
(def SEQ clojure.lang.LazySeq)
(def STR String)
(def TF Boolean)
(def CHR Character)
(def NUM BigDecimal)
(def DBL Double)
(def INT Long)
(def BIG BigInteger)

(defrecord CMD [x]
  Object
    (toString [_] x))
(defn CMD? [x] (instance? CMD x))

(defrecord FN [x f n]
  Object
    (toString [_] (str/join " " x)))
(def dFN (->FN (lazy-seq []) "" 0))
(defn eFN [x {{:keys [f n]} :code}] (->FN x f n))

;POLY

(defmulti show type)
(defmulti a-get type)
(defmulti toTF type)
(defmulti toCHR type)
(defmulti toNUM type)
(defmulti toDBL type)
(defmulti toINT type)
(defmulti toBIG type)

(defmethod show TF [x] (if x "$T" "$F"))
(defmethod show CMD [{x :x}] x)
(defmethod show FN [{:keys [f n]}] (str "(" f ":" n ")"))
(defmethod show SEQ [_] (str "[?]"))
(defmethod show ARR [xs] (str (map show xs)))
(defmethod show STR
  [x]
  (as-> x $
    (str/escape $ (assoc char-escape-string \" "\\\""))
    (str "\"" $ "\"")))
(defmethod show CHR
  [x]
  (as-> x $ (str/escape $ (assoc char-escape-string \' "\\'")) (str "'" $ "'")))
(defmethod show :default
  [x]
  (match x
    nil "UN"
    :else (str x)))

(defmethod toCHR NUM [x] (char x))
(defmethod toCHR Number [x] (char x))
(defmethod toCHR CHR [x] x)
(defmethod toCHR :default [x] (char (first x)))

(defmethod toNUM NUM [x] x)
(defmethod toNUM Number [x] (bigdec x))
(defmethod toNUM CHR [x] (toNUM (toINT x)))
(defmethod toNUM :default [x] (bigdec (str x)))

(defmethod toDBL DBL [x] x)
(defmethod toDBL Number [x] (double x))
(defmethod toDBL CHR [x] (toDBL (toINT x)))
(defmethod toDBL :default [x] (Double/parseDouble (str x)))

(defmethod toINT INT [x] x)
(defmethod toINT Number [x] (long x))
(defmethod toINT CHR [x] (long x))
(defmethod toINT :default [x] (Long/parseLong (str x)))

(defmethod toBIG BIG [x] x)
(defmethod toBIG Number [x] (bigint x))
(defmethod toBIG CHR [x] (bigint x))
(defmethod toBIG :default [x] (bigint (str x)))

(defmethod a-get SEQ [xs n] (nth xs (util/-i xs n) nil))
(defmethod a-get ARR [xs n] (get xs (util/-i xs n)))
(defmethod a-get FN [{x :x} n] (recur x n))
(defmethod a-get CMD [{x :x} n] (recur x n))
(defmethod a-get :default [xs n] (get xs n :not-found))

;UTIL

(defn numx [f & xs] (apply f (map toNUM xs)))

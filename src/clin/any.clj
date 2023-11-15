(ns clin.any
  (:require [clojure.string :as str]
            [clin.util :as util]
            [clojure.core.match :refer [match]]))

;CUSTOM

(def ARR clojure.lang.IPersistentVector)
(def Itr clojure.lang.Seqable)
(def SEQ clojure.lang.LazySeq)
(def STR String)
(def Num Number)
(def NUM BigDecimal)
(def DBL Double)
(def INT Long)
(def BIG BigInteger)
(def TF Boolean)
(def CHR Character)

(defrecord CMD [x]
  Object
    (toString [_] x))
(defn CMD? [x] (instance? CMD x))

(defrecord FN [x f n]
  Object
    (toString [_] (str/join " " x))
  Itr
    (seq [_] x))
(defn FN? [x] (instance? FN x))
(def dFN (->FN (lazy-seq []) "" 0))
(defn eFN [x {{:keys [f n]} :code}] (->FN x f n))

;POLY

(defmulti show type)
(defmulti toTF type)
(defmulti toNum type)
(defmulti toINT type)
(defmulti toCHR type)
(defmulti a-get type)

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
(defmethod show nil [_] "UN")
(defmethod show :default [x] (str x))

;TODO: toSTR? (join "" for seqs)
;TODO: vec

(defmethod toTF TF [x] x)
(defmethod toTF Num [x] (not= x 0))
(defmethod toTF Itr [x] (boolean (not-empty x)))
(defmethod toTF nil [_] false)
(defmethod toTF :default [x] (boolean x))

(defmethod toNum Num [x] x)
(defmethod toNum TF [x] (if x 1 0))
(defmethod toNum CHR [x] (long x))
(defmethod toNum nil [_] 0)
(defmethod toNum :default [x] (bigdec (str x)))

(defmethod toINT INT [x] x)
(defmethod toINT :default [x] (long (toNum x)))

(defmethod toCHR CHR [x] x)
(defmethod toCHR Num [x] (char x))
(defmethod toCHR nil [_] \u0000)
(defmethod toCHR :default [x] (recur (first x)))

(defmethod a-get SEQ [xs n] (nth xs (util/-i xs (toNum n)) :NF))
(defmethod a-get ARR [xs n] (get xs (util/-i xs (toNum n)) :NF))
(defmethod a-get FN [{x :x} n] (recur x n))
(defmethod a-get CMD [{x :x} n] (recur x n))
(defmethod a-get nil [_ _] :NF)
(defmethod a-get :default [xs n] (get xs n :NF))

;UTIL

(defn numx
  [f & xs]
  (->> xs
       (map toNum)
       (apply f)))

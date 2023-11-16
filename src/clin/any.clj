(ns clin.any
  (:require [clojure.string :as str]
            [clin.util :as util]))

;;; ALIASES

(def ARR clojure.lang.IPersistentVector)
(def Itr clojure.lang.Seqable)
(def Idx clojure.lang.Indexed)
(def Seq clojure.lang.ISeq)
(def SEQ clojure.lang.LazySeq)
(def STR String)
(def Num Number)
(def NUM BigDecimal)
(def DBL Double)
(def INT Long)
(def BIG BigInteger)
(def TF Boolean)
(def CHR Character)

;;; CUSTOM TYPES

(defrecord CMD [x]
  Object
    (toString [_] x))

(defn CMD? [x] (instance? CMD x))

(declare FN?)
(deftype FN [x f n]
  clojure.lang.ISeq
    (cons [_ o] (FN. (cons o x) f n))
    (empty [_] (FN. (empty x) f n))
    (equiv [_ {x1 :x, :as o}] (and (FN? o) (= x x1)))
    (first [_] (first x))
    (more [_] (FN. (rest x) f n))
    (next [_] (FN. (next x) f n))
    (seq [t] (if (seq x) t nil))
  Object
    (toString [_] (str x)))

(defn FN? [x] (instance? FN x))

(def dFN (->FN (lazy-seq []) "" 0))

(defn xFN [x t] (->FN x (.-f t) (.-n t)))

(defn eFN [x {code :code}] (xFN x code))

;;; HIERARCHY

(derive Itr ::Itrs)
(derive ::Idx ::Itrs)

(derive Idx ::Idx)
(derive ::Str ::Idx)

(derive STR ::Str)
(derive CMD ::Str)

;;; MULTIMETHODS

(defmulti show type)
(defmulti toTF type)
(defmulti toNum type)
(defmulti toINT type)
(defmulti toCHR type)
(defmulti vecz? type)
(defmulti a-get
  #(-> [(type %) (integer? %2)]))
(defmulti a-rem
  #(-> [(type %) (integer? %2)]))

(defmethod show TF [x] (if x "$T" "$F"))
(defmethod show CMD [{x :x}] x)
(defmethod show FN [{:keys [f n]}] (str "(" f ":" n ")"))
(defmethod show SEQ [_] (str "[?]"))
(defmethod show ARR [xs] (str (map show xs)))
(defmethod show STR
  [x]
  (-> x
      (str/escape (assoc char-escape-string \" "\\\""))
      (#(str "\"" % "\""))))
(defmethod show CHR
  [x]
  (-> x
      (str/escape (assoc char-escape-string \' "\\'"))
      (#(str "'" % "'"))))
(defmethod show nil [_] "UN")
(defmethod show :default [x] (str x))

;TODO: toSTR? (join "" for seqs)

(defmethod toTF TF [x] x)
(defmethod toTF Num [x] (not= x 0))
(defmethod toTF ::Itr [x] (boolean (not-empty x)))
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

(defmethod vecz? Itr [_] true)

(defmethod a-get [SEQ true] [xs n] (nth xs (util/-i xs n) nil))
(defmethod a-get [FN true] [t n] (recur (.-x t) n))
(defmethod a-get [::Idx true] [xs n] (get xs (util/-i xs n)))
(defmethod a-get [CMD ::default] [{x :x} n] (recur x n))
(defmethod a-get [::Idx false] [xs n] (recur xs (toNum n)))
(defmethod a-get :default [xs n] (get xs n))

(defmethod a-rem [SEQ true]
  [xs n]
  (let [[a b] (split-at (util/-i xs n) xs)] (lazy-cat a (rest b))))
(defmethod a-rem [FN true]
  [xs n]
  (-> xs
      .-x
      (a-rem n)
      (xFN xs)))
(defmethod a-rem [ARR true]
  [xs n]
  (let [i (util/-i xs n)] (into (subvec xs 0 i) (subvec xs (inc i)))))
(defmethod a-rem [STR true]
  [xs n]
  (let [i (util/-i xs n)] (str (subs xs 0 i) (subs xs (inc i)))))
(defmethod a-get [::Idx false] [xs n] (recur xs (toNum n)))
(defmethod a-rem :default [xs n] (recur (str xs) n))

;;; UTIL

(defn numx
  [f & xs]
  (->> xs
       (map toNum)
       (apply f)))

(defn vecz [n f & xs] ())

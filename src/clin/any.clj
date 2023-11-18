(ns clin.any
  (:require [clin.any.core :as ac :refer [ARR CHR Coll Idx INT Num SEQ STR TF]])
  (:import [clin.any.core CMD FN])
  (:require [clojure.string :as str]
            [clin.parser :as parser]
            [clin.util :as util]))

;;; HIERARCHY

(derive Coll ::Coll)
(derive ::Idx ::Coll)

(derive Idx ::Idx)
(derive ::Str ::Idx)
(derive ::Lazy ::Idx)

(derive SEQ ::Lazy)
(derive FN ::Lazy)

(derive STR ::Str)
(derive CMD ::Str)

(derive Coll ::Itr)
(underive ::Str ::Itr)
(underive FN ::Itr)

;;; MULTI

(defmulti show type)
(defmulti toTF type)
(defmulti toNum type)
(defmulti toINT type)
(defmulti toCHR type)
(defmulti toItr type)
(defmulti toSEQ type)
(defmulti toFNx type)
(defmulti toARR type)
(defmulti vecz? type)
(defmulti a-get
  #(-> [(type %) (integer? %2)]))
(defmulti a-rem
  #(-> [(type %) (integer? %2)]))
(defmulti a-map (fn [_ x] (type x)))

;;; METHODS
;TODO: toSTR? (join "" for seqs)

(defmethod show TF [x] (if x "$T" "$F"))
(defmethod show CMD [{x :x}] x)
(defmethod show FN [x] (str "(" (.-f x) ":" (.-n x) ")"))
(defmethod show SEQ [_] (str "[?]"))
(defmethod show ARR
  [xs]
  (->> xs
       (map show)
       (str/join " ")
       (#(str "[" % "]"))))
(defmethod show STR
  [x]
  (-> x
      (str/escape (assoc char-escape-string \" "\\\""))
      (as-> $ (str "\"" $ "\""))))
(defmethod show CHR
  [x]
  (-> x
      (str/escape (assoc char-escape-string \' "\\'"))
      (as-> $ (str "'" $ "'"))))
(defmethod show nil [_] "UN")
(defmethod show :default [x] (str x))

(defmethod toTF TF [x] x)
(defmethod toTF Num [x] (not= x 0))
(defmethod toTF ::Coll [x] (boolean (not-empty x)))
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

(defmethod toSEQ SEQ [x] x)
(defmethod toSEQ FN [x] (.-x x))
(defmethod toSEQ ::Coll [x] (lazy-seq x))
(defmethod toSEQ :default [x] (lazy-seq [x]))

(defmethod toFNx STR [x] (parser/parse x))
(defmethod toFNx :default [x] (toSEQ x))

(defmethod toARR ARR [x] x)
(defmethod toARR ::Coll [x] (vec x))
(defmethod toARR :default [x] (vec [x]))

(defmethod vecz? ::Itr [_] true)
(defmethod vecz? :default [_] false)

(defmethod a-get [SEQ true] [xs i] (nth xs (util/-i xs i) nil))
(defmethod a-get [FN true] [t i] (recur (.-x t) i))
(defmethod a-get [CMD true] [{x :x} i] (get x (util/-i x i)))
(defmethod a-get [::Idx true] [xs i] (get xs (util/-i xs i)))
(defmethod a-get [::Idx false] [xs i] (recur xs (toNum i)))
(defmethod a-get :default [xs i] (get xs i))

(declare xFN)

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
(defmethod a-rem [::Idx false] [xs n] (recur xs (toNum n)))
(defmethod a-rem :default [xs n] (recur (str xs) n))

(defmethod a-map SEQ [f xs] (map f xs))
(defmethod a-map FN [f xs] (xFN (map f xs) xs))
(defmethod a-map ARR [f xs] (mapv f xs))
(defmethod a-map :default [f xs] (recur f (toARR xs)))

;;; UTIL

(defn CMD? [x] (instance? CMD x))

(defn FN? [x] (instance? FN x))

(def dFN (FN. (lazy-seq []) "" 0 {}))

(defn xFN [x t] (FN. (toFNx x) (.-f t) (.-n t) (.-s t)))

(defn eFN [x {code :code}] (xFN x code))

(defn vecz
  [f & xs]
  (let [[v t l] (reduce (fn [[v t l] x]
                          (let [t1 (vecz? x)
                                v1 (if t1 x (repeat x))
                                l1 (isa? (type x) ::Lazy)]
                            [(conj v v1) (or t t1) (or l l1)]))
                  [[] false]
                  xs)]
    (if t
      (let [v1 (apply map #(apply vecz f %&) v)] (if l v1 (vec v1)))
      (apply f xs))))

(defn numx [f & xs] (apply vecz #(apply f (map toNum %&)) xs))

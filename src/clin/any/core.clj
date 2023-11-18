(ns clin.any.core
  (:import [clojure.lang Seqable Indexed ISeq LazySeq IPersistentVector
            IPersistentCollection]))

;;; ALIASES

(def Coll IPersistentCollection)
(def ARR IPersistentVector)
(def Idx Indexed)
(def SEQ LazySeq)
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

(deftype FN [x f n s]
  Seqable
    (seq [_] (seq x))
  IPersistentCollection
    (equiv [_ {x1 :x, :as o}] (and (instance? FN o) (= x x1)))
    (empty [_] (FN. (empty x) f n s))
    (count [_] (count x))
  ISeq
    (cons [_ o] (FN. (cons o x) f n s))
    (first [_] (first x))
    (more [_] (FN. (rest x) f n s))
    (next [_] (FN. (next x) f n s))
  Object
    (toString [_] (str x)))

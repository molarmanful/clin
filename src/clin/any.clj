(ns clin.any)

(defrecord NUM [x])
(defrecord STR [x])
(defrecord CMD [x])

(defmulti toNUM :type)
(defmulti wNUM :type)

(defmulti toSTR :type)
(defmulti wSTR :type)

(defmulti toCMD :type)
(defmulti wCMD :type)

(defmethod wNUM :default [x] (NUM x))

(defmethod wSTR :default [x] (STR x))

(defmethod wCMD :default [x] (CMD x))

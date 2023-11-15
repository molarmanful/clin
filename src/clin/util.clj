(ns clin.util)

(defn -i [x i] (if (neg? i) (- (count x) i) i))

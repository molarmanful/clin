(ns clin.core
  (:require [clin.env :as env])
  (:gen-class))

(defn -main [& _] (env/run "2 3 0.5 pop 1_ pick"))

(comment
  (use '[clin.env])
  (run "2 3 0.5 pop 1_ pick")
  ())

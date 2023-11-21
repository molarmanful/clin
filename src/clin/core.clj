(ns clin.core
  (:require [clin.any :as any]
            [clin.env :as env])
  (:gen-class))

(defn -main [& _] (env/run "( 1 2 3 (4 5 ()) 6 7)"))

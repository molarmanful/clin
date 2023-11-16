(ns clin.core
  (:require [clin.env :as env])
  (:gen-class))

(defn -main [& args] (env/run "2 3 5 0pick 1pick"))

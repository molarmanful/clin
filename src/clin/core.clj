(ns clin.core
  (:require [clin.env :as env])
  (:gen-class))

(defn -main [& args] (env/run "1_"))

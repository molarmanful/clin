(ns clin.main
  (:require [clin.parser :as parser])
  (:gen-class))

(defn -main [& args] (print (parser/parse "1234\"mafo\"asdf2+.3.xyz")))

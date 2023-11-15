(ns clin.core
  (:require [clin.env :as env])
  (:require [clj-commons.pretty.repl :as pretty.repl])
  (:gen-class))

(defn -main [& args] (pretty.repl/install-pretty-exceptions) (env/run "1_"))

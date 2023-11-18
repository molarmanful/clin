(require '[clin.any :as any] '[clin.env :as env])

(def e env/dENV)
(-> e
    (env/push [1 2 3] (any/eFN "1+" e))
    env/MAP)

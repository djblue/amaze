(ns amaze.build
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [shadow.cljs.devtools.api :as shadow]))

(defn -main []
  (binding [*out* *err*] (shadow/release :app))
  (println
   (s/replace
    (slurp "resources/index.html")
    "<script src=\"main.js\"></script>"
    (str "<script>" (slurp "target/main.js") "</script>"))))


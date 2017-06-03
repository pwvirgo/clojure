(ns slide.help 
  (:import (java.io File)))

(def usage
   "This app presents a slide show of images.\n\n
usage: slideshow dir where dir is the (full)
     path to the directory of images.\n")

(defn exitFail [& msgs]
  (println msgs "\n")
  (System/exit -13))

(defn msg [& msgs]  (println msgs "\n"))

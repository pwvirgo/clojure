(ns slide.images
  (:import (java.io File) (javax.imageio ImageIO))
  (:require [slide.help :as hlp])
  )

;(def imagelist (atom []))


(defn isDir? [dir]
  (if (.isDirectory (File. dir))
    true 
    (do
      (if  (.exists (File. dir))
        (hlp/msg  "help! " dir " is not a directory" "\n" hlp/usage)
        (hlp/msg  "help! " dir " does not exist\n" hlp/usage
                  ))
      false)))

(defn jpeg? [f] 
 (.endsWith (.toLowerCase (.getName  f)) ".jpg") )


(defn getImageFiles [dir]
  "returns a clojure.lang.PersistentVector of java.io.File s
    in directory dir (including contents of  subdirectories)"
  (if (isDir? dir)
    ;; (vec (file-seq (File. dir))) 
    (vec (->> (file-seq (File. dir))
              (filter jpeg?))) 


    ["failed to find any images"]))

(defn random-image [files]
  "returns a list with the filename and image from a vector
   of image Files"
  ;; (.getName (files (rand-int (count files))))
  (let [ifile  (files (rand-int (count files)))] 
    (list (.getName ifile)   (ImageIO/read ifile))))


;;  testing
(defn x [] 
  (getImageFiles "/Users/pwv/a/photos/2015/20150704DeRuyterNY"))


(random-image (x))

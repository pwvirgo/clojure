(ns slide.core
	(use (slide slideshow))
  (:gen-class))

(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
	
  
  (println "slideshow called with these arguments " args "\n")
  
  ;;(slideshow  "/Users/pwv/a/photos/2004/20050502_Colorado/"))
  (slideshow (first args))
  (println "core.clj saying bye " imagelist "\n\n")
  )

;;(def directory (clojure.java.io/file "/Users/pwv/a/photos/2004/"))
;;(def files (file-seq directory))
;;(print "wtf?")
;;(for [f [1 2 3]] (println f))
;;(print "wtf?")
;;(print (take 3 files))

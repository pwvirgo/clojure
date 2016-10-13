
(ns slide.slideshow
  (:import (java.io File)
	   (javax.imageio ImageIO)
	   (javax.swing JFrame JPanel Timer)
	   (java.awt Dimension Frame Color)))
 
(def imagelist (atom []))
(def running (atom true))

(defmacro props [obj & properties]
  (let [target (gensym)]
    `(let [~target ~obj]
       (vector ~@(for [property properties]
                   `(~property ~target))))))

(defn- jpeg? [f]
  (and  (.isFile f)
	(.endsWith (.toLowerCase (.getName  f)) ".jpg")))

(defn populate-imagelist [dir]
  (reset! imagelist (->> (file-seq (File. dir))
        		 (filter jpeg?)
			 (map #(.getPath %)))))

(defn center [image-dims region-dims]
  (vec (map #(- (/ %2 2) (/ %1 2)) image-dims region-dims)))
 
(defn scale [factor dims]
  (vec (map #(* factor %) dims)))
 
(defn compute-bounds [image-dims region-dims]
  (let [scaling (apply min 1 (map / region-dims image-dims))]
    [(center (scale scaling image-dims) region-dims)
     scaling]))
 
(defn fit-to [image-dims region-dims]
  (let [[[x y] scaling] (compute-bounds image-dims region-dims)
	[width height] (map #(int (* % scaling)) image-dims)] 
    [x y width height]))

(defn random-image-path []
    (when-let [list (seq @imagelist)]
        (nth list (rand-int (count list)))))
 
(defn random-image []
  (when-let [image-path (random-image-path)]
    (ImageIO/read (File. image-path))))

(defn paint [g]
  (.setColor g Color/black)
  (let [region-rect   (.getClipBounds g)
	[rw rh]       (props region-rect .getWidth .getHeight)
	current-image (random-image)]
    (.fillRect g 0 0 rw rh)
    (if current-image
      (let [image-dims (props current-image .getWidth .getHeight)
	    [x y width height] (fit-to image-dims [rw rh])]
	(.drawImage g current-image x y width height (Color. 0 0 0) nil))
      (doto g
	(.setColor Color/white)
	(.drawString "Working..." 800 600)))))
 
(defn make-panel []
  (proxy [JPanel] []
    (paintComponent   [g] (paint g))))

(defn start-imagelist-population [dir]
  (doto (Thread. #(populate-imagelist dir))
    (.setPriority Thread/MIN_PRIORITY)
    .start))

(defn update-ui [p]
  (while @running
	 (.repaint p)
	 (Thread/sleep 5000)))

(defn slideshow [dir]
  (println "hello from slideshow")
  (start-imagelist-population dir)
  (println @imagelist)
  (let [panel (make-panel)]
    (doto (JFrame. "Slideshow")
      (.setExtendedState Frame/MAXIMIZED_BOTH)
      (.setUndecorated true)
      .pack .show
      (.add panel))
    (future (update-ui panel))))

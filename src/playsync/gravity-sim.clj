(def dim 200)

(defstruct body :mass :velocity)

(defstruct cell :contains)

(def bcount 10)

(def g 3)

(def update-time 30)                                        ;anim update time ms

(def animation-sleep-ms 30)

(def world
  (vec (map (fn [_]
              (vec (map (fn [_]
                          (ref (struct cell false)))
                        (range dim))))
            (range dim))))

(defn place
  [[x y]]
  (nth (nth world x) y))

(defn create-body
  [mass location velocity]
  (dosync
    (let [p (place location)
          ag (struct body mass velocity)]
      (alter p assoc :body ag)
      (alter p assoc :contains true)
      (agent location))))

(defn setup
  "Initializes our fake world"
  []
  (dosync
    (doall
      (for [i (range bcount)]
        (let [x (rand-int dim) y (+ (/ dim 2) (rand-int (/ dim 2)))]
          (create-body 10 [x y] 0))))))

(defn delta-mov
  [[x y]]
  (let [oldp (place [x y])
        utime (/ update-time 1000)
        dx 0
        dy (+ (* utime (-> @oldp :body :velocity)) (* 0.5 g (* utime utime)))
        ]
    (if (< (- y dy) 0)
      [(+ x dx) 0]
      [(+ x dx) (- y dy)])))

(defn delta-velocity
  [[x y]]
  (let [oldp (place [x y])
        v (-> @oldp :body :velocity)
        utime (float (/ update-time 1000))
        ]
    (println "utime" utime v)
    (+ v (* g utime))))

(defn move
  "moves the object in direction"
  [loc]
  (let [oldp (place loc)
        body (:body @oldp)
        v (delta-velocity loc)
        newloc (delta-mov loc)
        [nx ny] (map #(int (+ 0.5 %)) newloc)
        p (place [nx ny])
        ]
    ;(println newloc)
    (if (= loc [nx ny])
      (dosync
        (alter p assoc :body body)
        (alter p assoc-in [:body :velocity] v)
        [nx ny])
      (dosync
        ;move the body
        (alter p assoc :body body)
        (alter oldp dissoc :body)
        ;update body params
        (alter p assoc-in [:body :velocity] v)
        (alter oldp assoc :contains false)
        [nx ny]))
    ))

(defn dtest
  [[x y]]
  (if (= y 0)
    false
    true))

(defn behave
  "the main function for body agent"
  [loc]
  (println  loc)
  (let [p (place loc)
        body (:body @p)
        ]
    ;(println body @p)
    (Thread/sleep update-time)
    (dosync
      (when (dtest loc)
        (send-off *agent* #'behave))
      ;(println "after agent" *agent*)
      (move loc))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import
  '(java.awt Color Graphics Dimension)
  '(java.awt.image BufferedImage)
  '(javax.swing JPanel JFrame))

;pixels per world cell
(def scale 5)

(defn fill-cell [#^Graphics g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn render-body [ant #^Graphics g x y]
  (doto g
    (.setColor (new Color 255 0 0 255))
    (.drawRect (* x scale) (* scale (- dim y))
               (* 3 scale) (* 3 scale))))

(defn render-place [g p x y]
  (when (:body p)
    (render-body (:body p) g x y)))

(defn render [g]
  (let [v (dosync (apply vector (for [x (range dim) y (range dim)]
                                  @(place [x y]))))
        img (new BufferedImage (* scale dim) (* scale dim)
                 (. BufferedImage TYPE_INT_ARGB))
        bg (. img (getGraphics))]
    (doto bg
      (.setColor (. Color white))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight))))
    (dorun
      (for [x (range dim) y (range dim)]
        (render-place bg (v (+ (* x dim) y)) x y)))
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(def panel (doto (proxy [JPanel] []
                   (paint [g] (render g)))
             (.setPreferredSize (new Dimension
                                     (* scale dim)
                                     (* scale dim)))))

(def frame (doto (new JFrame) (.add panel) .pack .show))

(def animator (agent nil))

(defn animation [x]
  (when true
    (send-off *agent* #'animation))
  (. panel (repaint))
  (. Thread (sleep animation-sleep-ms))
  nil)

;;driver code

(def bodies (setup))
(send-off animator animation)
;(dorun (map #(send-off %  behave) bodies))
(send-off (first bodies) behave)
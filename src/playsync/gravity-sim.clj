(def dim 100)

(defstruct body :mass :location)

(defstruct cell :contains)

(def bcount 10)

(def world
  (vec (map (fn [_]
              (vec (map (fn [_]
                          (ref (struct cell false)))
                        (range dim))))
            (vec (range dim)))))

(defn place
  [[x y]]
  (nth (nth world x) y))

(defn create-body
  [mass location]
  (dosync
    (let [p (place location)
          ag (struct body mass location)]
      (alter p assoc :body ag)
      (alter p assoc :contains true)
      (agent location))))

(defn setup
  "Initializes our fake world"
  []
  (dosync
    (dotimes [i bcount]
      (let [x (rand-int dim) y (+ 40 (rand-int 30))]
        (create-body 10 [x y]))))
  )
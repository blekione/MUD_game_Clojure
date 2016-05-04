(ns fun-pro.data)

(def descriptions {:1 '(You are in the living room)
                   :2 '(You are in the hallway)
                   :3 '(You are in a swamp)
                   })

(def objects ["a silver dagger"
              "a gold coin"
              "a rusty sword"
              "a viking helmet"
              "a steel long sword"
              "a healing potion"
              "a plate with meat meal (+2 strength)"])


(def look {:directions :look
           :look :look
           :examine :look
           })

(def quit {:exit :quit
           :quit :quit
           })

(def pick {:get :pick
           :pickup :pick
           :pick :pick})

(def put {:put :drop
          :drop :drop
          :place :drop
          :remove :drop})

(def inventory {:inventory :inventory
                :bag :inventory})

(def decisiontable {:1 {:north :2, :west :3}
                    :2 {:south :1}
                    :3 {}})

(def actions (merge look quit pick put inventory))

;; merge actions into every record of decisiontable - result is list of maps with single key-value pair
(def merged-tables
  (map #(merge (assoc {} (first %) (merge (second %) actions))) decisiontable))

;; converting list of maps with single key-value pair into single map
(defn list-to-map [input-lst]
  (zipmap (map #(first (first %)) input-lst) (map #(second (first %)) input-lst)))

(def final-decision-table
  (list-to-map merged-tables))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; databases for rooms and inventory  and associated functions to work with it

;;;;; function to create mutable references (atom) to vector for each room

(defn create-object-db [input-coll]
  (loop [x 1]
    (intern *ns* (symbol (str "room" x "-db")) (atom []) )
    (if (< x (count input-coll))
      (recur (+ x 1))
      (intern *ns* (symbol (str "inventory-db")) (atom []) ))))


;;;;; functions to adds/remove objects to db

(defn find-item [input-str coll]
  (first (filter #(.contains input-str %) objects))) ;; use java.lang.String.contains method

(defn add-item [output-db item]
  (if (nil? item)
      false
      (swap! output-db conj item)))

(defn remove-item [input-db item]
  (if (nil? item)
      false
      (swap! input-db #(vec (remove #{item} %)))))

;;;;; adds up to 4 random items from the list to the room-db

(defn add-items [room-db]
  (loop [x (rand-int 4) my-objects objects ]
    (when (> x 0)
      (let [random-item (rand-nth my-objects)]
        (print (str "item: " random-item))
        (add-item room-db random-item)
        (recur (- x 1)
               (remove #{random-item} my-objects)))
     ))
  @room-db) 

;;;;; display objects from database

(defn display-objects [db]
  (let [data @db
        output-no-first (clojure.string/join ", " (rest data))
        output (if (> (count data) 1)
                 (str output-no-first " and " (first data))
                 (first data))]
    (when (not-empty output)
      (println (str output)))))

;;;;; moving object between db

(defn move-object [input-db output-db input-str]
  (let [item (find-item input-str input-db)]
    (if (complement (nil? item))
       (do
         (add-item output-db item)
         (remove-item input-db item))
       )))


(ns fun-pro.data)

(def descriptions {:1 '(You are in the living room)
                   :2 '(You are in the hallway)
                   :3 '(You are in a swamp)
                   })

(def objects {:1 "a silver dagger"
              :2 "a gold coin"
              :3 "a rusty sword"
              :4 "a viking helmet"
              :5 "a steel long sword"
              :6 "a healing potion"
              :7 "a plate with meat meal (+2 strength)"})


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
;; maps for player inventory  and associated functions to work with it

(def inventory-db {})

;; function to create mutable maps for each room with use of references

(defn create-object-db [input-coll]
  (loop [x 1]
    (intern *ns* (symbol (str "room" x "-db")) (ref {:room true}) )
    (if (< x (count input-coll))
      (recur (+ x 1))
      (intern *ns* (symbol (str "inventory-db")) (ref {:room false}) ))))

;; adds items from defined list of objects to room-object-db

(defn add-item-to-room [room-db item-id]
  (let [size (count (deref room-db))
        item-key (keyword (str (+ size 1)))]
    (dosync (alter room-db assoc item-key item-id))
    ))

(defn remove-item-from-room [room-db item-key]
  (dosync (alter room-db dissoc item-key)))


;; adds up to 4 random items from the list to the room-db

(defn add-items [room-db]
  (loop [x (rand-int 4) items-keys (keys objects) ]
    (when (> x 0)
      (let [random-item (rand-nth items-keys)]
        (print (str "item: " random-item))
        (add-item-to-room room-db random-item)
        (recur (- x 1)
               (remove #{random-item} (keys objects))))
     ))
  @room-db) 

;; display objects from mutable maps

(defn display-objects [db]
  (let [db-objects-keys (rest (vals @inventory-db))
        output-no-first (clojure.string/join ", " (map #(get objects %) (rest db-objects-keys)))
        output (if (> (count db-objects-keys) 1)
                 (str output-no-first " and " (get objects (first db-objects-keys)) )
                 (get objects (first db-objects-keys)))]
    (when (not-empty output)
      (if (get db :room)
        (println (str "You can see " output))
        (println (str "You are carrying " output))))))

;; moving item from room to inventory

(defn move-from-room-to-inventory [db id]
  (let [object-id (get (deref db) id)
        object-des (get objects object-id)]
    (cond
     (contains? (deref db) id)
      (do
       (println "Added" object-des "to your bag")
       (add-item-to-room inventory-db object-id)
       (remove-item-from-room db id))
     :else (println "I don't see that item in the room!"))))

(defn move-from-inventory-to-room [db id]
  (let [object-id (get (deref inventory-db) id)
        object-des (get objects object-id)]
    (cond
     (contains? (deref inventory-db) id)
      (do
       (println "Removed" object-des "from your bag")
       (add-item-to-room db object-id)
       (remove-item-from-room inventory-db id))
     :else (println "You are not carrying that item!"))))

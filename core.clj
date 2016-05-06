(ns fun-pro.core
  (require [fun-pro.data :refer :all])
  (require [fun-pro.objects :refer :all]))
(require ['clojure.string :as 'string])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn check-if-direction [value]
  (some #(= value %) '(:north :south :east :west)))
(defn get-directions [id]
  (let [record (get final-decision-table id)
        result (filter #(check-if-direction (first %)) record)
        n (count result)]
    (case n
      0 (println "It appears you have entered a room with no exits.>")
      1 (println (format "You can see an exit to the %s>" (name (first (first result)))))
      (let [lst-of-keywords (map first result)
            lst-of-strings (map name lst-of-keywords)]
        (println (str "You can see exits to the " (string/join " and " lst-of-strings) ">"))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns list of keywords from decisiontable "attached" to id 
(defn get-keywords [id]
  (let [keys (get final-decision-table id)]
    (map (fn [key] (first key)) keys)))

;; lookup if decision-table contains any tokens and returns associated value
(defn lookup-clojure [id tokens]
  (let [set-value (into #{} (get-keywords id))
           tokens (into #{} (map keyword tokens))
           set (clojure.set/intersection tokens set-value)]
    (cond 
      (or (empty? set) (> (count set) 1)) :false  
      :else (get (get final-decision-table id) (first set)))))

(defn String->Number [str]
  (let [n (read-string str)]
       (if (number? n) n nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn startgame-1 [initial-id]
  (let [object-dbs (create-object-db descriptions)] ;; collection of db for objects in rooms and inventory
    (loop [x (rest object-dbs)] 
      (if (seq x) 
        (do
          (add-items (first x))
          (recur (rest x)))))
    (loop [id initial-id description true]
      (let [room-db (nth object-dbs (Integer/parseInt (name id)))
            inventory-db (first object-dbs)]
        (if description
          (do
            (println (str "You are in "(clojure.string/join " "(get descriptions id)) "> "))
            (if (not-empty @room-db)
              (println (str "Yoo can see " (display-items room-db)))
              (println "It seems like nothing interesting is here"))))
      (let [input (read-line)
            tokens (string/split input #" ")
            response (lookup-clojure id tokens)
            room-item (find-item input room-db)
            inventory-item (find-item input inventory-db)
            reply (cond
                   (= response :false)
                     "I dont understand>"            
                   (= response :look)
                     (get-directions id) 
                   (= response :quit)
                     "So Long, and Thanks for All the Fish...>"
                   (= response :pick)
                     (if (nil? room-item)
                       (println "I can't see such an item in the room")
                       (do
                         (move-item room-db inventory-db input)
                         (println (str "You picked " room-item " to your bag"))))
                   (= response :drop)
                     (if (nil? inventory-item)
                       (println "I can't find such an item in your bag")
                       (do
                         (move-item inventory-db room-db input)
                         (println (str
                                   "You dropped "
                                   inventory-item " in "
                                   (clojure.string/join " " (get descriptions id))))))
                   (= response :inventory)
                     (if (empty? @inventory-db)
                       (println "Your bag is empty")
                       (println (str "You have " (display-items inventory-db) " in your bag"))))]
        (if (not= reply nil)
          (println reply))
        (cond 
            (number? (String->Number (name response)))
              (recur response true)
            (= response :room-info)
              (recur id true)
            (not= response :quit)
              (recur id false))
        ))
      ))  
  )

(ns fun-pro.core)
(require ['clojure.string :as 'string])


(def descriptions {:1 '(You are in the living room)
                   :2 '(You are in the hallway)
                   :3 '(You are in a swamp)
                   })


(def look {"directions" :look
           "look" :look
           "examine room" :look
           })

(def quit {"exit game" :quit
           "quit game" :quit
           "exit" :quit
           "quit" :quit
           })

(def decisiontable {:1 {:north :2, :west :3}
                    :2 {:south :1}
                    :3 {}})

(def actions (merge look quit))

;; merge actions into every record of decisiontable - result is list of maps with single key-value pair
(def merged-tables
  (map #(merge (assoc {} (first %) (merge (second %) actions))) decisiontable))

;; converting list of maps with single key-value pair into single map
(defn list-to-map [input-lst]
  (zipmap (map #(first (first %)) input-lst) (map #(second (first %)) input-lst)))


(def directions {:1 {:north :2, :south :0, :east :0, :west :0}
                 :2 {:north :0, :south :1, :east :0, :west :0}})

;; check which direction are available
(defn lookup [room-id direction]
 (get (get directions room-id ) direction))


;; function to start game
(defn startgame [room-id]
  (loop [rid room-id]
    (println (clojure.string/join " "(get descriptions rid)))
    (let [input (keyword (read-line))]
      (cond
       (= input :quit) "bye bye"
       (some #(= input %) '(:north :south :east :west) )
       (let [direction (lookup rid input)]
         (if (= :0 direction)
           (recur rid)
           (recur direction)))
       :else (do
               (print "uh! I don't understand. Stil, ")
               (recur rid)))      
      )))

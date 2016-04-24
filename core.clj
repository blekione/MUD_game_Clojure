(ns fun-pro.core)
(require ['clojure.string :as 'string])


(def descriptions {:1 '(You are in the living room)
                   :2 '(You are in the hallway)
                   :3 '(You are in a swamp)
                   })

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

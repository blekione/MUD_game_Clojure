(ns fun-pro.core)

(def descriptions { :1 '(you are in the living room)
                    :2 '(you are in the hallway)
                   })

(def directions { :1 {:north :2, :south :0, :east :0, :west :0}
                  :2 {:north :0, :south :1, :east :0, :west :0}})

(defn lookup [room-id direction]
 (get (get directions room-id ) direction))

(defn startgame [room-id]
  (loop [rid room-id]
    (println (clojure.string/join " "(get descriptions rid)))
    (let [input (keyword (read-line))]
      (cond
       (= input :quit) "bye bye"
       (some #(= input %) '(:north :south :east :west) )
       (let [direction (lookup rid input)]
         (if (= 0 direction)
           (recur rid)
           (recur direction)))
       :else (recur input))      
      ))) 
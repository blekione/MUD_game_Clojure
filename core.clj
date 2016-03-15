(ns fun-pro.core)

(def descriptions { :1 '("you are in the living room")
                    :2 '("you are in the hallway")
                   })

(def directions { :1 {:north 2, :south 0, :east 0, :west 0}
                  :2 {'north 0, 'south 1, 'east 0, 'west 0}})

(defn get-room-description [id]
  (get descriptions id))

(defn lookup [room-id direction]
 (get (get directions room-id ) direction))

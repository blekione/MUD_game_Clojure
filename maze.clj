(ns fun-pro.maze)
 

(defn empty-maze [x y]
  (loop [a 0 coll {}]
                (if (<= a x)
                  (recur (+ a 1) 
                         (loop [b 0 coll coll]
                           (if (<= b y)
                             (recur (+ b 1) (assoc coll (keyword (str a b)) {}))
                             coll)))
                  coll)))

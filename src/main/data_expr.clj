(defn- vector-handle [data current]
  (let [first# (first current) count# (count current)]
    (condp #(%1 %2) first#
      number?
      (condp = count#
        1 (subvec data first#)
        2 (subvec data first# (second current))
        (throw (IllegalArgumentException. "number type size max is 2")))
      fn?
      (condp = count#
        1 (first# data)
        2 (first# (second current) data)
        3 (first# (second current) (nth current 2) data)
        4 (first# (second current) (nth current 2) (nth current 3) data)
        5 (first# (second current) (nth current 2) (nth current 3) (nth current 4) data)
        6 (first# (second current) (nth current 2) (nth current 3) (nth current 4) (nth current 5) data)
        (throw (IllegalArgumentException. "fn type maximum number of parameters is 5")))
      (throw (IllegalArgumentException. (str "unsupported type " (type current)))))
    )
  )
(defn draw [data expr]
  (if
    (or (nil? data) (empty? expr))
    data
    (let [current (first expr)]
      (recur
        (condp #(%1 %2) current
          number? (nth data current)
          string? (get data current)
          symbol? (get data (name current))
          keyword? (current data)
          fn? (current data)
          vector? (vector-handle data current)
          (throw (IllegalArgumentException. (str "unsupported type " (type current)))))
        (rest expr))
      )
    )
  )

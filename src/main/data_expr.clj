(defn- vector-handle [data expr]
  (let [first# (first expr) count# (count expr)]
    (condp #(%1 %2) first#
      number?
      (condp = count#
        1 (subvec data first#)
        2 (subvec data first# (second expr))
        (throw (IllegalArgumentException. "number type size max is 2")))
      fn?
      (condp = count#
        1 (first# data)
        2 (first# (second expr) data)
        3 (first# (second expr) (nth expr 2) data)
        4 (first# (second expr) (nth expr 2) (nth expr 3) data)
        5 (first# (second expr) (nth expr 2) (nth expr 3) (nth expr 4) data)
        6 (first# (second expr) (nth expr 2) (nth expr 3) (nth expr 4) (nth expr 5) data)
        (throw (IllegalArgumentException. "fn type maximum number of parameters is 5")))
      (throw (IllegalArgumentException. (str expr))))
    )
  )


(defn draw [data expr]
  (if
    (or (nil? data) (empty? expr))
    data
    (let [expr# (first expr)]
      (recur
        (condp #(%1 %2) expr#
          number? (nth data expr#)
          string? (get data expr#)
          symbol? (get data (name expr#))
          keyword? (expr# data)
          fn? (expr# data)
          vector? (vector-handle data expr#)
          (throw (IllegalArgumentException. (str expr))))
        (rest expr))
      )
    )
  )

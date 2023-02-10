


(defn batch> [f]
  #(map f %))
(defn stream> [origin handles]
  (if (nil? handles)
    origin
    (if (coll? handles)
      (loop [origin origin handles handles]
        (if (empty? handles)
          origin
          (recur ((first handles) origin) (rest handles))))
      (handles origin))))

(defn read-format-text [^String text keys & {:keys [| =>] :or {| " " => identity}}]
  (let [action (if keys zipmap #(identity %2))]
    (->
      (for [line (clojure.string/split-lines text)]
        (->> | re-pattern (clojure.string/split line) (action keys))) (#(stream> % =>)))
    ))

(defmacro read-format-file [text keys & body]
  `(read-format-text (slurp ~text) ~keys ~@body))


(defmacro def-filter-fn [fn-name default-decision]
  `(defn ~fn-name
     ([value] (eq identity value))
     ([key value] (let [key-fn (if (fn? key) key #(get % key))
                        value-fn (if (fn? value) value #(~default-decision % value))]
                    (fn [datalist] (filter #(-> % key-fn value-fn) datalist)))))
  )
(def-filter-fn eq =)
(def-filter-fn ne not=)
(def-filter-fn lt <)
(def-filter-fn gt >)
(def-filter-fn le <=)
(def-filter-fn ge >=)

(defn order [key-fn]
  (fn [datalist] (sort-by key-fn datalist)))

(defn group [key-fn]
  (fn [datalist] (group-by key-fn datalist)))

(defn limit
  ([size] (limit 0 size))
  ([offset size] (fn [datalist] (subvec (vec datalist) offset (+ offset size)))))

(defn batch [f]
  (fn [datalist] (map f datalist))
  )


(comment "demo"
         (read-format-text
           (str/join "\n" ["1 张三 12" "2 李四 9" "3 王五 20"])
           [:id :name :amount]
           :| " "
           :=>
           [(batch #(update % :amount bigdec))
            (ge :amount 10)])
)

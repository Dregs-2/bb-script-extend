(ns curl)

(require '[clojure.string :as str])
(require '[babashka.fs :as fs])
(require '[cheshire.core :as json])

(defn -headers [headers]
  (map #(apply format "--header '%s: %s'" %) (update-keys headers name)))

(defn -request [method]
  (let [method (str/upper-case (name method))]
    (assert (#{"HEAD" "GET" "POST" "PUT" "PATCH" "DELETE" "COPY" "OPTIONS" "LINK" "UNLINK" "PURGE" "LOCK" "UNLOCK" "PROPFIND" "VIEW"} method))
    (if (= "HEAD" method)
      "--head"
      (format "--request %s" method)
      )
    )
  )

(defn -query-params [data]
  (if (empty? data)
    ""
    (str "?" (str/join "&" (map #(format "%s=%s" (java.net.URLEncoder/encode (name (key %)) "UTF-8") (java.net.URLEncoder/encode (str (val %)) "UTF-8")) data))))
  )

(defn -url [schema host port uri query-params]
  (format "%s://%s:%s%s%s" schema host port (#(if (str/starts-with? % "/") % (str "/" %)) (or uri "")) (-query-params query-params))
  )

(def -file #(some-> % str fs/expand-home fs/file fs/real-path str))

(defn -binary [path]
  (when path
    (let [real-path (-file path)]
      (when real-path
        [{} (format "--data-binary '@%s'" path)]
        )
      )
    )
  )

(def -raw-json #(when % [{"Content-Type" "application/json"} (format "--data-raw '%s'" (if (string? %) % (json/encode %)))]))
(def -raw-text #(when % [{"Content-Type" "text/plain"} (format "--data-raw '%s'" %)]))
(def -raw #(when % [{} (format "--data-raw '%s'" %)]))


(defn -format-data [template data]
  (mapv #(format template (name (key %)) (name (val %))) (-> data (or {})))
  )

(defn -form-urlencoded [data]
  (when data
        [{"Content-Type" "application/x-www-form-urlencoded"}
         (-format-data "--data-urlencode '%s=%s'" data)])
  )

(defn -form-data [data]
  ;--form 'a="s"' \
  ;--form 'd="c"' \
  ;--form 'asd=@"/path/to/file"' \
  ;--form '=@"/path/to/file"' \
  ;--form '=@"/path/to/file"'
  )


(defn curl
  "[method schema host port & {:keys [uri query-params headers form-data form-urlencoded binary raw raw-json raw-text] :or {:uri \"\" :headers {}}}]"
  [method schema host port & {:keys [uri query-params headers form-data form-urlencoded binary raw raw-json raw-text] :or {:uri "" :headers {}} :as opts}]
  (assert (>= 1 (count (filter true? (map (complement nil?) [raw raw-json raw-text binary form-urlencoded form-data])))))
  (let [request (-request method)
        url (-url schema host port uri query-params)
        binary (-binary binary)
        raw-json (-raw-json raw-json)
        raw-text (-raw-text raw-text)
        raw (-raw raw)
        form-urlencoded (-form-urlencoded form-urlencoded)
        form-data (-form-data form-data)
        payload (or binary raw-json raw-text raw form-urlencoded form-data)
        headers (-headers (merge headers (first payload)))]
    (str/join " \\\n" (filterv (complement nil?) (concat [(format "curl --location %s '%s'" request url)] headers ((if (coll? (second payload)) identity vector) (second payload)))))
    )
  )

(defn curl+ "[method url & opts]" [method url & opts]
  (let [url (java.net.URI/create url)
        scheme (.getScheme url)
        host (.getHost url)
        port (.getPort url)
        path (.getPath url)]
    (apply curl (concat [method scheme host port :uri (or path "")] (vec opts)))
    )
  )

[#'curl #'curl+]

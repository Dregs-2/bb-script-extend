;brew install gum
;choose     Choose an option from a list of choices
;confirm    Ask a user to confirm an action
;file       Pick a file from a folder
;filter     Filter items from a list
;format     Format a string using a template
;input      Prompt for some input
;join       Join text vertically or horizontally
;pager      Scroll through a file
;spin       Display spinner while running a command
;style      Apply coloring, borders, spacing to text
;table      Render a table of data
;write      Prompt for long-form text

(babashka.deps/add-deps '{:deps {io.github.lispyclouds/bblgum {:git/sha "7ebae0e2231899fe2a6ad44bc9ef5fca64099fcd"}}})
(require '[bblgum.core :as b])

(def ^:dynamic interceptor identity)

(def ^:dynamic style-flag {})

(def gum #(interceptor (b/gum {:cmd %1 :as %2 :args %3 :in %4 :opts (apply assoc (cons {} (apply concat (filter val (merge style-flag %5)))))})))

(defmacro def-opt [opt cmd args m]
  (let [keys (vec (map #(symbol (name %)) (keys m)))
        opts (zipmap (map keyword keys) keys)
        params (rest args)
        args (first args)
        m (update-keys m #(symbol (name %)))]
    `(defn ~opt [~@args & {:keys ~keys :or ~m}]
       (gum ~cmd ~@params ~opts)
       )
    )
  )
(def-opt choose> :choose [[args] nil args nil] {:limit             nil
                                                :no-limit          (not limit)
                                                :height            20
                                                :cursor            "> "
                                                :cursor-prefix     "[ ] "
                                                :selected-prefix   "[✓] "
                                                :unselected-prefix "[ ] "})

(def-opt confirm> :confirm [[text] :bool text nil] {:affirmative "Yes"
                                                    :negative    "No"
                                                    :default     true
                                                    :timeout     0})


(def-opt file> :file [[path] nil path nil] {:cursor    ">"
                                            :all       true
                                            :file      true
                                            :directory false
                                            :height    0})

(def-opt input> :input [[] nil nil nil] {:placeholder ""
                                         :prompt      "> "
                                         :value       ""
                                         :char-limit  0
                                         :width       40
                                         :password    false
                                         :header      ""})

(def-opt table> :table [[content] nil nil (when-not (instance? java.io.File content) content)] {:separator ","
                                                                                                :columns   nil
                                                                                                :widths    nil
                                                                                                :height    20
                                                                                                :file      (some->> (when (instance? java.io.File content) content) fs/real-path str)})
(def-opt write> :write [[] nil nil nil] {:width             50
                                         :height            20
                                         :header            ""
                                         :placeholder       ""
                                         :prompt            "┃ "
                                         :show-cursor-line  true
                                         :show-line-numbers true
                                         :value             ""
                                         :char-limit        0})
(def-opt filter> :filter [[text] nil nil text] {:limit             nil
                                                :no-limit          true
                                                :indicator         "•"
                                                :selected-prefix   "[✓]"
                                                :unselected-prefix "[ ]"
                                                :placeholder       "Filter..."
                                                :prompt            "> "
                                                :width             50
                                                :height            20
                                                :value             ""
                                                :reverse           false
                                                :fuzzy             nil
                                                :no-fuzzy          nil
                                                :strict            nil
                                                :no-strict         nil
                                                })
(def-opt spin> :spin [[] nil nil nil] {:show-output false
                                       :title       "Loading..."
                                       :align       "left"
                                       :spinner     "dot"})
(def-opt pager> :pager [[text] :ignored nil text] {:show-line-numbers true
                                                   :soft-wrap         true})

;(def-opt format> :format [[] nil nil nil] {})
;(def-opt join> :join [[] nil nil nil] {})
;(def-opt style> :style [[] nil nil nil] {})

(comment
  (defn choose> [args]
    (gum :choose nil args nil
         {limit             nil
          no-limit          true
          height            20
          cursor            "> "
          cursor-prefix     "[ ] "
          selected-prefix   "[✓] "
          unselected-prefix "[ ] "})
    )

  (defn confirm> [text]
    (gum :confirm :bool text nil
         {:affirmative "Yes"
          :negative    "No"
          :default     true
          :timeout     0})
    )
  (defn file> [path]
    (gum :file nil path nil
         {:cursor    ">"
          :all       true
          :file      true
          :directory false
          :height    0})
    )

  (defn input> []
    (gum :input nil nil nil
         {:placeholder ""
          :prompt      "> "
          :value       ""
          :char-limit  0
          :width       40
          :password    false
          :header      ""})
    )

  (defn table> [content]
    (let [file? (instance? java.io.File content)]
      (gum :table nil nil (when-not file? content)
           {:separator ","
            :columns   nil
            :widths    nil
            :height    20
            :file      (some->> content (when file?) fs/real-path str)})
      )
    )

  (defn write> []
    (gum :write nil nil nil
         {:width             50
          :height            20
          :header            ""
          :placeholder       ""
          :prompt            "┃ "
          :show-cursor-line  true
          :show-line-numbers true
          :value             ""
          :char-limit        0}))

  (defn filter> [text]
    (gum :filter nil nil text
         {:limit                         nil
          :no-limit                      true
          :indicator                     "•"
          :selected-prefix               "[✓]"
          :unselected-prefix             "[ ]"
          :placeholder                   "Filter..."
          :prompt                        "> "
          :width                         50
          :height                        20
          :value                         ""
          :reverse                       false
          (if true :fuzzy :no-fuzzy)     true
          (if false :strict :no-:strict) true}))

  (defn spin> []
    (gum :spin nil nil nil
         {:show-output false
          :title       "Loading..."
          :align       (if true "left" "right")
          :spinner     (#{"line", "dot", "minidot", "jump", "pulse", "points", "globe", "moon", "monkey", "meter", "hamburger"} "dot")}))

  (defn pager> [text]
    (gum :pager :ignored nil text
         {:show-line-numbers true
          :soft-wrap         true}))


  ;(defn format> [])
  ;(defn join> [])
  ;(defn style> [])
  )



(def def-jump-exception #(new RuntimeException %))

(def *130-exception (def-jump-exception "exit-130"))

(defn finish [args atom-index atom-contexts]
  (swap! atom-index inc)
  (swap! atom-contexts #(conj % args))
  )

(defn jump [target-index atom-index atom-contexts]
  (when (and (<= 0 target-index) (< target-index @atom-index))
    (reset! atom-index target-index)
    (swap! atom-contexts #(subvec % 0 target-index))
    )
  )

(defn capable-of-fallback-thread [arg fns]
  (binding [interceptor #(let [{status :status result :result} %]
                           (when (= 130 status) (throw *130-exception)) result)]
    (let [break (atom false)
          index (atom 0)
          contexts (atom [])]
      (while
        (and (not @break) (< @index (count fns)))
        (try
          (let [$index @index
                arg (if (zero? $index) arg (nth @contexts (dec $index)))
                result ((nth fns $index) arg)]
            (finish result index contexts))
          (catch RuntimeException e
            (when-not (identical? *130-exception e) (throw e))
            (if (zero? @index)
              (swap! break not)
              (jump (dec @index) index contexts))
            )
          )
        )
      )
    )
  )



(comment "demo"
  (defn fn1 [args]
    (choose> args :limit 1)
    )
  (defn fn2 [args]
    (-> args first str ((juxt #(str % 1) #(str % 2) #(str % 3))) (choose> :limit 1))
    )
  (defn fn3 [args]
    (-> args first str ((juxt #(str % 1) #(str % 2) #(str % 3))) (choose> :limit 1))
    )

  (defn fn4 [args]
    (let [[arg] args]
      (str arg "-" (first (input>)))
      )
    )

  (defn fn5 [args]
    (pager> args)
    )

  (capable-of-fallback-thread [1 2 3] [fn1 fn2 fn3 fn4 fn5])
  )

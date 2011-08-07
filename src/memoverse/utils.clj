(ns memoverse.utils
  (import [java.awt GridBagConstraints Insets]
	  [javax.swing WindowConstants])
  (use [clojure.contrib.def]))

(defvar *default-close-operation* WindowConstants/DISPOSE_ON_CLOSE)

(defmacro keep-bindings [bindings f]
  (let [bind-vars (take (count bindings) (repeatedly gensym))]
    `(let [~@(interleave bind-vars bindings)]
       (fn [& args#]
         (binding [~@(interleave bindings bind-vars)]
           (apply ~f args#))))))

(defn make-constraint
  "Make constraint for GridBagLayout"
  [fill gridx gridy weightx weighty]
  (let [c (GridBagConstraints.)]
    (set! (. c fill) (condp = fill
		       :horizontal GridBagConstraints/HORIZONTAL
		       :vertical GridBagConstraints/VERTICAL
		       :both GridBagConstraints/BOTH
		       GridBagConstraints/NONE))
    (set! (. c gridx) gridx)
    (set! (. c gridy) gridy)
    (set! (. c weightx) weightx)
    (set! (. c weighty) weighty)
    c))

(defn jvm-version-gt?
  "Detect that JVM has at least the given version"
  [version]
  (let [system-version-seq (map #(Integer. %)
				(re-seq #"\d+" (System/getProperty "java.version")))
	user-version-seq (map #(Integer. %)
			      (re-seq #"\d+" version))]
    (condp = (first (drop-while #(= :equal %)
				(map (fn [system-digit user-digit]
				       (cond
					 (> system-digit user-digit) :greater
					 (= system-digit user-digit) :equal
					 :else :less))
				     system-version-seq
				     user-version-seq)))
      :greater true
      nil true ;; this is when user version matches the JVM version
      false)))

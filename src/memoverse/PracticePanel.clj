(ns memoverse.PracticePanel
  (import [javax.swing JPanel JTextPane JToggleButton JLabel JProgressBar Timer]
	  [java.awt Color GridBagLayout Dimension]
	  [java.awt.event ActionListener KeyListener KeyEvent]
	  [javax.swing.text StyleConstants StyleContext])
  (use [memoverse.utils]
       [clojure.contrib.def])
  (require [clojure.contrib.str-utils2 :as str]
	   [clojure.contrib.seq-utils :as sq]))

(defvar *font-size* 20
  "Default font size for the practice panel")
(defvar *timeout* (long 1000)
  "Timer timeout in miliseconds")
(defvar *completed-string*
  ["Well done!",
   "Congratulations!",
   "Not bad!",
   "Good job, keep it up!"])

(defn- default-style
  "Return the default style"
  []
  (doto (.getStyle (StyleContext/getDefaultStyleContext) StyleContext/DEFAULT_STYLE)
    (StyleConstants/setFontSize *font-size*)
    (StyleConstants/setBold false)
    (StyleConstants/setForeground Color/black)))

(defn- wrong-style
  "Mark wrong attempts with red"
  []
  (doto (default-style)
    (StyleConstants/setForeground Color/red)
    (StyleConstants/setBold true)))

(defn- correct-style
  "Mark correct attempts with green"
  []
  (doto (default-style)
    (StyleConstants/setForeground Color/green)))

(defn- stable-style
  "Mark stable string with black"
  []
  (doto (default-style)
    (StyleConstants/setForeground Color/black)))

(defn- replace-doc-at-pos
  "Given a StyledDocument, replace the character at pos with replacement using style"
  [doc pos replacement style]
  (doto doc
    (.remove pos 1)
    (.insertString pos replacement style)))
  
(defn practice-panel
  "Create a practice panel:
  - reference is used as the title of the panel
  - category is the category of verse chosen by the user
  - text is the text being tested
  - difficulty represents the probability that a character is hidden
  - verses-set is just a string showing the current practice set (e.g 3/10)"
  [reference category text difficulty verses-set]
  (let [text-pane (JTextPane.)
	doc (.getStyledDocument text-pane)
	;; completed is a ref to a vector of booleans, each boolean representing
	;; a correctly entered character
	completed (ref 
		   (apply concat
			  (map (fn [word] 
				 (repeat (count word)
					 (cond
					   ;; always display punctuation marks
					   (re-find #"[ ;:,.'\"!?)(\[\]-]" word) true
					   ;; weigh word according to difficulty
					   (> (rand) difficulty) true
					   :else false)))
			       (filter #(not= "" %)
				       (str/split text #"\b+")))))
	progress (JProgressBar. 0 100)
	elapsed-label (JLabel. "Elapsed 0 secs")
	elapsed-time (atom 0)
	toggle-button (JToggleButton.)]
    (letfn [;; redraw the text panel (may be called by a timer task)
	    (redraw 
	     []
	     (.grabFocus text-pane)
	     (.remove doc 0 (.getLength doc))
	     (doseq [[idx letter] (partition 2 (interleave (range (count text))
							   @completed))]
	       (.insertString doc 
			      (.getLength doc)
			      (cond (.isSelected toggle-button) (str (nth text idx))
				    letter (str (nth text idx))
				    :else "_")
			      (stable-style))))		
	    ;; process the user entered key
	    (process-key
	     [k]
	     (when (and (not (.isSelected toggle-button))
			(some #(false? %) @completed))
	       (let [curr-position (ffirst (filter (fn [[idx letter]]
						     (if letter false true))
						   (partition 2 (interleave (range (count text))
									    @completed))))
		     correct-key (str (nth text curr-position))]
		 (if (= (.toLowerCase correct-key) (.toLowerCase k))
		   ;; correct key
		   (do (ref-set completed (concat (take curr-position @completed)
						  [true]
						  (drop (inc curr-position) @completed)))
		       (replace-doc-at-pos doc curr-position correct-key (correct-style))
		       (.setValue progress (float (* 100 (/ (count (filter #(true? %) @completed))
							    (count text)))))
		       (when (every? #(true? %) @completed)
			 (.setString progress (sq/rand-elt *completed-string*))
			 (.setText toggle-button "Click here to continue...")))
		   ;; incorrect key
		   (replace-doc-at-pos doc curr-position k (wrong-style))))))]
      (let [timer (doto (Timer. *timeout* (proxy [ActionListener] []
					    (actionPerformed [e] (redraw))))
		    (.setRepeats false))]
	(doto (Timer. (long 1000) 
		      (proxy [ActionListener] []
			(actionPerformed 
			 [e] 
			 (swap! elapsed-time inc)
			 (when-not (every? #(true? %) @completed)
			   (.setText elapsed-label (format "Elapsed %d secs" @elapsed-time))))))
	  (.setRepeats true)
	  (.start))
	(redraw)
	(doto (JPanel. (GridBagLayout.))
	  (.setPreferredSize (Dimension. 500 200))
	  (.add (JLabel. (format "Reference: %s Category: %s (Difficulty %.0f%%) - Practicing %s verse(s)" 
				 reference category (* 100 difficulty) verses-set))
		(make-constraint :horizontal 0 0 1 0))
	  (.add (doto text-pane
		  (.setEditable false)
		  (.addKeyListener
		   (proxy [KeyListener] []
		     (keyPressed [e] nil)
		     (keyReleased 
		      [e]
		      ;; FSM:
		      ;; 1. Cancel current timer
		      ;; 2. Redraw this panel
		      ;; 3. Process new key
		      ;; 4. Schedule new timer for redraw
		      (when (condp = (.getKeyCode e)
			      KeyEvent/VK_BACK_SPACE false
			      KeyEvent/VK_SPACE false
			      KeyEvent/VK_TAB false
			      KeyEvent/VK_ENTER false
			      true)
			(dosync
			 (.stop timer)
			 (redraw)
			 (process-key (str (.getKeyChar e)))
			 (.restart timer))))
		     (keyTyped [e] nil))))
		(make-constraint :both 0 1 1 1))
	  (.add (doto progress 
		  (.setStringPainted true)
		  (.setValue (float (* 100 (/ (count (filter #(true? %) @completed))
					      (count text))))))
		(make-constraint :horizontal 0 2 1 0))
	  (.add (doto toggle-button
		  (.setText "Take a peek...")
		  (.addActionListener (proxy [ActionListener] []
					(actionPerformed 
					 [e]
					 ;; once verse is complete, this button is double-dutied
					 (let [b (.getSource e)]
					   (if (every? #(true? %) @completed)
					     (.dispose (.getTopLevelAncestor b))
					     (do
					       (.setText b (if (.isSelected b)
							     "Ok, I'm ready..."
							     "Take a peek..."))
					       (redraw))))))))
		(make-constraint :horizontal 0 3 1 0))
	  (.add elapsed-label
		(make-constraint :vertical 0 4 1 0)))))))


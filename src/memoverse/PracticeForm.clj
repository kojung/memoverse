(ns memoverse.PracticeForm
  (import [javax.swing JPanel JComboBox JLabel JFrame JRadioButton JButton JDialog
	   ButtonGroup WindowConstants]
	  [java.awt GridBagLayout Dimension]
	  [java.awt.event ActionListener WindowListener])
  (use [memoverse.utils]
       [memoverse.db]
       [memoverse.PracticePanel]
       [clojure.contrib.def])
  (require [clojure.contrib.str-utils2 :as str]
	   [clojure.contrib.seq-utils :as sq]))

(defvar *verse-difficulty-increment* 5
  "Increment difficulty each time a verse is practiced")

(defn- process-practice-form
  "Recursively process the content of practice form:
  - db is the database
  - N = # of verses to practice
  - n = current verse
  - difficulty = integer or :auto
  - category = category of verses to practice"
  [db N n difficulty category]
  (when (<= n N)
    (let [[reference _ prev-difficulty content] 
	  (sq/rand-elt (filter (fn [[_ this-category _ _]]
				 (if (or (= category "all")
					 (= this-category category))
				   true
				   false))
			       (verses db)))]
      (doto (JFrame. "Practicing")
	(.setDefaultCloseOperation WindowConstants/DISPOSE_ON_CLOSE)
	(.add (practice-panel reference
			      category
			      content 
			      (float (/ (if (= :auto difficulty) 
					  (+ 10 prev-difficulty)
					  difficulty)
					100))
			      (format "%s/%s" n N)))
	(.pack)
	(.setVisible true)
	(.addWindowListener 
	 (proxy [WindowListener] []
	   (windowActivated [e] nil)
	   (windowClosed 
	    [e]
	    (update-db (change-verse-difficulty db reference
						(+ *verse-difficulty-increment*
						   prev-difficulty)))
	    (process-practice-form (get-db) N (inc n) difficulty category))
	   (windowClosing [e] nil)
	   (windowDeactivated [e] nil)
	   (windowDeiconified [e] nil)
	   (windowIconified [e] nil)
	   (windowOpened [e] nil)))))))
			    
(defn practice-form
  "Create a practice form"
  [db parent]
  (let [verses-combo (doto (JComboBox. (into-array ["1" "2" "4" "6" "8" "10" "all"]))
		       (.setSelectedIndex 2))
	categories-combo (doto (JComboBox. (into-array (concat ["all"] (categories db))))
			   (.setSelectedIndex 0))
	auto-button (JRadioButton. "auto" true)
	manual-button (JRadioButton. "manual")
	difficulty-mode (atom :auto)
	difficulty-combo (doto (JComboBox. (into-array ["10% (easy)" "15%" "25%" "50% (medium)" "75%" "100% (hard)"]))
			   (.setEnabled false))]
    (doto (ButtonGroup.)
      (.add (doto auto-button
	      (.addActionListener (proxy [ActionListener] []
				    (actionPerformed
				     [e]
				     (reset! difficulty-mode :auto)
				     (.setEnabled difficulty-combo false))))))
      (.add (doto manual-button
	      (.addActionListener (proxy [ActionListener] []
				    (actionPerformed
				     [e]
				     (reset! difficulty-mode :manual)
				     (.setEnabled difficulty-combo true)))))))
    (doto (JDialog. parent "Practice")
      (.setResizable false)
      (.setDefaultCloseOperation WindowConstants/DISPOSE_ON_CLOSE)
      (.add 
       ;; main panel
       (doto (JPanel. (GridBagLayout.))
	 ;; instructions
	 (.add (JLabel. "Please choose the number of verses and difficulty: ")
	       (make-constraint :horizontal 0 0 0 0))
	 ;; # of verses panel / categories
	 (.add 
	  (doto (JPanel. (GridBagLayout.))
	    (.add (JLabel. "Number of verses: ")
		  (make-constraint :horizontal 0 0 0 0))
	    (.add verses-combo
		  (make-constraint :horizontal 1 0 0 0))
	    (.add (JLabel. "Categories: ")
		  (make-constraint :horizontal 2 0 0 0))
	    (.add categories-combo
		  (make-constraint :horizontal 3 0 0 0)))
	  (make-constraint :horizontal 0 1 0 0))
	 ;; difficulty panel
	 (.add
	  (doto (JPanel. (GridBagLayout.))
	    (.add (JLabel. "Difficulty: ")
		  (make-constraint :horizontal 0 0 0 0))
	    (.add auto-button
		  (make-constraint :horizontal 1 0 0 0))
	    (.add manual-button
		  (make-constraint :horizontal 2 0 0 0))
	    (.add difficulty-combo
		  (make-constraint :horizontal 3 0 0 0)))
	  (make-constraint :horizontal 0 2 0 0))
	 ;; practice
	 (.add (doto (JButton. "Start practice")
		 (.addActionListener (proxy [ActionListener] []
				       (actionPerformed 
					[e]
					(let [[val1 val2 val3] (map #(.getSelectedItem %)
								    [verses-combo difficulty-combo categories-combo])
					      total-verses (count (verses db))]
					  (process-practice-form db
								 (if (= val1 "all") 
								   total-verses
								   (min (Integer. val1) total-verses))
								 1 ; start with the first verse
								 (if (= :auto @difficulty-mode)
								   :auto
								   (Integer. (re-find #"\d+" val2)))
								 val3))
					(.dispose (.getTopLevelAncestor (.getSource e)))))))
	       (make-constraint :horizontal 0 3 0 0))))
      (.pack)
      (.setResizable false)
      (.setVisible true))))
  
	    
	    
	    
	    

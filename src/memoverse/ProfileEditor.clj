(ns memoverse.ProfileEditor
  (import [javax.swing JFrame JScrollPane JPanel WindowConstants JTextArea JTextField
	   ListSelectionModel JTable JButton JLabel JProgressBar AbstractCellEditor JDialog]
	  [javax.swing.table AbstractTableModel TableCellEditor]
	  [java.awt Color GridBagLayout Dimension]
	  [java.awt.event ActionListener MouseListener])
  (use [memoverse.utils]
       [memoverse.db]
       [clojure.contrib.def])
  (require [clojure.contrib.str-utils2 :as str]
	   [clojure.contrib.seq-utils :as sq]))

(defn- alter-elt-at
  "Functionally change the element at row/col in the 2D matrix"
  [matrix row col new-val]
  (loop [my-matrix matrix
	 my-row 0
	 new-matrix nil]
    (if-not (empty? my-matrix)
      (recur (rest my-matrix)
	     (inc my-row)
	     (conj new-matrix (let [this-row (first my-matrix)]
				(if (= my-row row)
				  (concat (take col this-row) [new-val] (drop (inc col) this-row))
				  this-row))))
      (reverse new-matrix))))

(defn profile-editor
  "Use a JTable to edit the profile"
  [db parent]
  (let [lst (atom (verses db))
	table (JTable. (proxy [AbstractTableModel] []
			 (getRowCount [] (count @lst))
			 (getColumnCount [] 4)
			 (getValueAt [row col] (nth (nth @lst row) col))
			 (isCellEditable [row col] true)
			 (setValueAt [val row col] (do (reset! lst (alter-elt-at @lst row col val))
						       (.fireTableCellUpdated this row col)))
			 (getColumnClass [col] (condp = col
						 2 Integer
						 3 JTextArea
						 String))
			 (getColumnName [col] (nth ["Reference" "Category" "Difficulty (0-100)" "Text"] col))))
	editor-dialog (JDialog. parent "Edit Profile")]
    ;; embed verse-editor in here so that it has access to the variables defined above
    (letfn [(verse-editor
	     []
	     (let [user-entered-text (atom "")]
	       (proxy [AbstractCellEditor TableCellEditor] []
		 (getTableCellEditorComponent
		  [table value is-selected row col]
		  (let [cell-editor this
			text-area (JTextArea. 6 20)
			model-row (if (jvm-version-gt? "1.6")
				    (.convertRowIndexToModel table row)
				    row)
			model-col (if (jvm-version-gt? "1.6")
				    (.convertColumnIndexToModel table col)
				    col)
			current-content (nth (nth @lst model-row) model-col)]
		    (doto (JTextField. value)
		      (.setEditable false)
		      (.addMouseListener 
		       (proxy [MouseListener] []
			 (mouseClicked
			  [e]
			  ;; activate editor with double-click
			  (when (= 2 (.getClickCount e))
			    (doto (JDialog. editor-dialog "Edit verse content")
			      (.add 
			       (doto (JPanel. (GridBagLayout.))
				 (.add (JScrollPane. text-area)
				       (make-constraint :both 0 0 1 1))
				 (.add (doto (JButton. "Done")
					 (.addActionListener (proxy [ActionListener] []
							       (actionPerformed 
								[e] 
								(reset! lst (alter-elt-at @lst 
											  model-row
											  model-col 
											  (.getText text-area)))
								(.dispose (.getTopLevelAncestor (.getSource e)))))))
				       (make-constraint :horizontal 0 1 1 0))))
			      (.pack)
			      (.setVisible true))
			    (doto text-area
			      (.setLineWrap true)
			      (.setWrapStyleWord true)
			      (.setText current-content))
			    (.fireEditingStopped cell-editor)))
			 (mouseEntered [e] nil)
			 (mouseExited [e] nil)
			 ;; update atom when user clicks the text-field, so that
			 ;; getCellEditorValue doesn't corrupt the data (super hack!!)
			 (mousePressed [e] (reset! user-entered-text current-content))
			 (mouseReleased [e] nil))))))
		 (getCellEditorValue [] @user-entered-text))))]
      ;; adjust table column width, giving preference to verse content
      (doseq [col (range 4)]
	(.. table getColumnModel (getColumn col) (setPreferredWidth (condp = col
								      0 50      ; reference
								      1 50      ; category
								      2 25      ; difficulty
								      3 300)))) ; content
      ;; allow sorting and single row selection
      (when (jvm-version-gt? "1.6")
	(.setAutoCreateRowSorter table true))
      (doto table
	(.setSelectionMode ListSelectionModel/SINGLE_SELECTION)
	(.setDefaultEditor JTextArea (verse-editor)))
      ;; create the profile editor frame
      (doto editor-dialog
	(.setDefaultCloseOperation WindowConstants/DISPOSE_ON_CLOSE)
	(.add 
	 (doto (JPanel. (GridBagLayout.))
	   ;; JTable
	   (.add (JScrollPane. table)
		 (let [c (make-constraint :both 0 0 1 1)]
		   (set! (. c gridwidth) 4)
		   c))
	   ;; Buttons
	   (.add (doto (JButton. "Add verse")
		   (.addActionListener (proxy [ActionListener] []
					 (actionPerformed 
					  [e]
					  (swap! lst conj ["new verse" "none" 10 "Enter text here..."])
					  (.fireTableRowsInserted (.getModel table) 
								  (dec (count @lst))
								  (dec (count @lst)))))))
		 (make-constraint :horizontal 0 1 1 0))
	   (.add (doto (JButton. "Delete verse")
		   (.addActionListener (proxy [ActionListener] []
					 (actionPerformed 
					  [e]
					  (let [row (if (jvm-version-gt? "1.6")
						      (.convertRowIndexToModel table
									       (.getSelectedRow table))
						      (.getSelectedRow table))]
					    (reset! lst (concat (take row @lst) (drop (inc row) @lst)))
					    (.fireTableRowsDeleted (.getModel table) row row))))))
		 (make-constraint :horizontal 1 1 1 0))
	   (.add (doto (JButton. "Cancel")
		   (.addActionListener (proxy [ActionListener] []
					 (actionPerformed [e] (.dispose (.getTopLevelAncestor (.getSource e)))))))
		 (make-constraint :horizontal 2 1 1 0))
	   (.add (doto (JButton. "Save")
		   (.addActionListener (proxy [ActionListener] []
					 (actionPerformed 
					  [e] 
					  (save-db (verses->db db @lst))
					  (open-db (:filename db)) ; re-open so that integer gets parsed correctly
					  (.dispose (.getTopLevelAncestor (.getSource e)))))))
		 (make-constraint :horizontal 3 1 1 0))
	   (.setPreferredSize (Dimension. 500 300))))
	(.pack)
	(.setVisible true)))))
  

(ns memoverse.core
  (:gen-class)
  (:import [javax.swing JPanel JLabel JFrame JButton WindowConstants Timer 
	    JFileChooser ImageIcon JOptionPane ImageIcon]
	   [java.awt Insets Color GridBagConstraints BorderLayout GridBagLayout Dimension]
	   [java.awt.event ActionListener]
	   [java.util.prefs Preferences])
  (:require [clojure.contrib.str-utils2 :as str2])
  (:use [clojure.contrib.def]      
	[memoverse.utils]
	[memoverse.db]
	[memoverse.PracticeForm]
	[memoverse.PracticePanel]
	[memoverse.ProfileEditor]
	[memoverse.PrintCards]))

(def *called-from-main* (atom false))

(defn- release-number
  "Return the release number for this app. Use the information in the Manifest"
  []
  (try 
   (if-let [release (.getSpecificationVersion (.getPackage memoverse.memoverse))]
     release
     "BETA")
   (catch Exception _ "BETA")))

(defvar *app-name* "MemoVerse")
(defvar *header* "<html>
<center>
<p><i>\"Thy word have I hid in mine heart, <br>
that I might not sin against thee. Psa 119:11\"</i></p>
<br><p>May this tool help you memorize God's word.</p>
<br>
</center>
</html>")
(defvar *footer* "<html>
<center>
<br>Questions? Comments?
<br><p>http://memoverse.konline.org</p><br>
</center>
</html>")

(defn- default-new-db
  "Returns a new db for first time user"
  []
  {:filename nil,
   :save false,
   :db {"John 3:16" ["salvation" 10 "For God so loved the world, that he gave his only begotten Son, that whosoever believeth in him should not perish, but have everlasting life."]}})

(defn- open-profile-file-chooser
  "File chooser dialog asking the location of the profile"
  [parent]
  (let [fc (JFileChooser.)
	retval (. fc showOpenDialog parent)]
    (when (= retval JFileChooser/APPROVE_OPTION)
      (open-db (.. fc getSelectedFile getCanonicalPath)))))

(defn- new-profile-file-chooser
  "File chooser dialog asking the location of the new profile being created"
  [parent]
  (let [fc (JFileChooser.)
	retval (. fc showSaveDialog parent)]
    (when (= retval JFileChooser/APPROVE_OPTION)
      (save-db (default-new-db) (.. fc getSelectedFile getCanonicalPath)))))

(defn- first-time-dialog
  [parent]
  (let [options (to-array ["Create new", "Open existing", "Cancel"])]
    (condp = (JOptionPane/showOptionDialog 
	      parent
	      "Looks like this is the first time you are using MemoVerse.\nYou will need a profile containing the verses that you wish to memorize.\nSample profiles can be downloaded from MemoVerse's website.\nHow would you like to proceed?"
	      "Welcome, first time user!"
	      JOptionPane/YES_NO_CANCEL_OPTION
	      JOptionPane/QUESTION_MESSAGE
	      nil
	      options
	      (aget options 0))
      0 (new-profile-file-chooser parent)
      1 (open-profile-file-chooser parent)
      2 nil)))
      
(defn show-gui
  "Display the main GUI"
  []
  (let [edit-profile-btn (JButton. (ImageIcon. (ClassLoader/getSystemResource "memoverse/icons/edit.jpg")))
	save-profile-btn (JButton. (ImageIcon. (ClassLoader/getSystemResource "memoverse/icons/save.jpg")))
	practice-btn (JButton. (ImageIcon. (ClassLoader/getSystemResource "memoverse/icons/practice.jpg")))
	print-cards-btn (JButton. (ImageIcon. (ClassLoader/getSystemResource "memoverse/icons/print.jpg")))
	main-frame (JFrame. *app-name*)]
    ;; create a timer to monitor the status of the DB
    (doto (Timer. 100 (proxy [ActionListener] []
			(actionPerformed
			 [e]
			 ;; edit/practice/print depends on an opened db
			 (doseq [button [edit-profile-btn practice-btn print-cards-btn]]
			   (.setEnabled button (if (get-db) true false)))
			 ;; save profile depends on a modified db
			 (.setEnabled save-profile-btn
				      (cond
					;; no opened db
					(nil? (get-db)) false					
					;; unmodified db
					(and (get-db) (:saved (get-db))) false
					;; modified db
					(and (get-db) (not (:saved (get-db)))) true))
			 ;; update main frame's title
			 (.setTitle main-frame
				    (cond
				      ;; no opened db
				      (nil? (get-db))
				      *app-name*
				      ;; unmodified db
				      (and (get-db) (:saved (get-db)))
				      (format "%s (%s)" *app-name* (release-number))
				      ;; modified db
				      (and (get-db) (not (:saved (get-db))))
				      (format "%s (%s) (modified)" 
					      *app-name* (release-number)))))))
      (.setRepeats true)
      (.start))
    ;; fetch last-opened profile, if it exists
    (if-let [profile-name (.get (Preferences/userNodeForPackage memoverse.core) "last-profile" nil)]
      (try (open-db profile-name)
	   (catch Exception _ (first-time-dialog main-frame)))
      (first-time-dialog main-frame))
    ;; Main program's JFrame
    (doto main-frame
      (.setDefaultCloseOperation *default-close-operation*)
      (.setBackground Color/WHITE)
      (.add 
       (doto (JPanel. (GridBagLayout.))
	 (.setBackground Color/WHITE)
	 ;; Banner and blurb goes here
	 (.add (JLabel. (ImageIcon. (ClassLoader/getSystemResource "memoverse/icons/banner.jpg")
				    "MemoVerse")
			JLabel/CENTER)
	       (let [c (make-constraint :horizontal 0 0 1 0)]
		 (set! (. c gridwidth) 3)
		 c))
	 (.add (JLabel. *header*)
	       (let [c (make-constraint :vertical 0 1 1 0)]
		 (set! (. c gridwidth) 3)
		 (set! (. c insets) (Insets. 5 10 5 10))
		 c))
	 ;; Open profile button
	 (.add (doto (JButton. (ImageIcon. (ClassLoader/getSystemResource "memoverse/icons/open.jpg")))
		 (.addActionListener (proxy [ActionListener] []
				       (actionPerformed
					[e]
					(open-profile-file-chooser main-frame)))))
	       (make-constraint :none 0 2 0 0))
	 (.add (JLabel. "Open Profile")
	       (make-constraint :vertical 0 3 1 1))
	 ;; Save profile button
	 (.add (doto save-profile-btn
		 (.addActionListener (proxy [ActionListener] []
				       (actionPerformed
					[e]
					(save-db (get-db))))))
	       (make-constraint :none 1 2 0 0))
	 (.add (JLabel. "Save Profile")
	       (make-constraint :vertical 1 3 1 1))
	 ;; Practice button
	 (.add (doto practice-btn
		 (.addActionListener (proxy [ActionListener] []
				       (actionPerformed
					[e]
					(practice-form (get-db) main-frame)))))
	       (make-constraint :none 2 2 0 0))
	 (.add (JLabel. "Practice")
	       (make-constraint :vertical 2 3 1 1))
	 ;; Edit profile button
	 (.add (doto edit-profile-btn
		 (.addActionListener (proxy [ActionListener] []
				       (actionPerformed
					[e]
					(profile-editor (get-db) main-frame)))))
	       (make-constraint :none 0 4 0 0))
	 (.add (JLabel. "Edit Profile")
	       (make-constraint :vertical 0 5 1 1))
	 ;; Edit profile button
	 (.add (doto print-cards-btn
		 (.addActionListener (proxy [ActionListener] []
				       (actionPerformed
					[e]
					(print-cards (get-db))))))
	       (make-constraint :none 1 4 0 0))
	 (.add (JLabel. "Print Cards")
	       (make-constraint :vertical 1 5 1 1))
	 ;; Quit program button
	 (.add (doto (JButton. (ImageIcon. (ClassLoader/getSystemResource "memoverse/icons/quit.jpg")))
		 (.addActionListener (proxy [ActionListener] []
				       (actionPerformed
					[e]
					(letfn [(conditional-exit 
						 []
						 (if @*called-from-main*
						   (System/exit 0)
						   (.dispose (.getTopLevelAncestor (.getSource e)))))]
					  ;; make sure profile is saved
					  (if (and (get-db)
						   (not (:saved (get-db))))
					    (let [ret (JOptionPane/showConfirmDialog main-frame
										     "Profile has been modified and has not been saved. Continue?"
										     "Unsaved profile warning"
										     JOptionPane/YES_NO_OPTION)]
					      (when (= ret JOptionPane/YES_OPTION)
						(conditional-exit)))
					    (conditional-exit)))))))
	       (make-constraint :none 2 4 0 0))
	 (.add (JLabel. "Quit Program")
	       (make-constraint :vertical 2 5 1 1))
	 (.add (JLabel. *footer*)
	       (let [c (make-constraint :vertical 0 6 1 0)]
		 (set! (. c gridwidth) 3)
		 c))))
      (.pack)
      (.setResizable false)
      (.setVisible true))))

(defn -main
  [ & cmd-args ]
  (binding [*default-close-operation* WindowConstants/EXIT_ON_CLOSE]
    (reset! *called-from-main* true)
    (show-gui)))

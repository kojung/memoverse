(ns memoverse.db
  (:gen-class)
  (:import [java.util.prefs Preferences])
  (:use [clojure.contrib.def])
  (:require [clojure.xml :as xml]
	    [clojure.zip :as zip]
	    [clojure.contrib.zip-filter.xml :as zf]
	    [clojure.contrib.prxml :as pr]
	    [clojure.contrib.duck-streams :as duck]))

(defvar *DB* (atom nil) "Atom holding the current DB")

(defn update-db
  "Update the current DB"
  [db]
  (reset! *DB* db))

(defn get-db
  "Return the current opened DB"
  []
  @*DB*)

(defn open-db
  "Open the XML database and return a map containing:
  {:filename filename
   :db {reference => [category difficulty content],...,}}.
  The atom *DB* will be updated as well, so that we have a chance to modify it later."
  [db-filename]
  (let [zipper (zip/xml-zip (xml/parse db-filename))
	lst (partition 4
		       (interleave (zf/xml-> zipper :verses :verse (zf/attr :reference))
				   (zf/xml-> zipper :verses :verse (zf/attr :category))
				   (map #(read-string %)
					(zf/xml-> zipper :verses :verse (zf/attr :difficulty)))
				   (zf/xml-> zipper :verses :verse zf/text)))]
    (update-db {:filename db-filename,
		:saved true,
		:db (loop [db {},
			   my-lst lst]
		      (if-let [[reference category difficulty content] (first my-lst)]
			(recur (assoc db reference [category difficulty content])
			       (rest my-lst))
			db))})
    (.put (Preferences/userNodeForPackage memoverse.core) "last-profile" db-filename)
    (get-db)))

(defn verses->db
  "Functionally change all the verses in the db with new verses represented as list of lists"
  [db new-verses]
  (assoc db
    :db (loop [v new-verses
	       m {}]
	  (if-let [[reference category difficulty content] (first v)]
	    (recur (rest v) (assoc m reference [category difficulty content]))
	    m))
    :saved false))

(declare verses)
(declare verse-by-reference)
(defn save-db
  "Save the db into the XML file. If filename is not provided, use the filename in the db.
   Return the same DB, but mark the saved flag as true.
   Also, update the user preferences for future use."
  ([db xml-file]
     (with-open [out (duck/writer xml-file)]
       (binding [*out* out]
	 (pr/prxml [:decl! "1.0"])
	 (pr/prxml 
	  [:memoverse
	   [:verses
	    (map (fn [[reference category difficulty content]]
		   [:verse {:reference reference,
			    :category category,
			    :difficulty difficulty}
		    content])
		 (verses db))]])))
     (update-db (assoc db :saved true :filename xml-file))
     (.put (Preferences/userNodeForPackage memoverse.core) "last-profile" xml-file)
     (get-db))
  ([db] (save-db db (:filename db))))

(defn change-verse-difficulty
  "Return a new db with the difficulty of the verse modified"
  [db reference new-difficulty]
  (let [[category _ content] (verse-by-reference db reference)]
    (assoc db
      :db (assoc (:db db) reference [category new-difficulty content])
      :saved false)))
     
(defn verses
  "Return a list of verses as [reference category difficulty content]"
  [db]
  (map (fn [[reference [category difficulty content]]]
	 [reference category difficulty content])
       (:db db)))
  
(defn categories
  "Return a set of categories used in the db"
  [db]
  (set (map (fn [[reference [category difficulty content]]]
	      category)
	    (:db db))))

(defn references
  "Return a set of references used in the db"
  [db]
  (keys (:db db)))

(defn verse-by-reference
  "Return the verse given the reference"
  [db reference]
  (get (:db db) reference nil))


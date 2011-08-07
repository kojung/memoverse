(ns memoverse.PrintCards
  (:import [javax.swing BorderFactory JButton JPanel JFrame RepaintManager JLabel WindowConstants]
	   [java.awt Color GridLayout GridBagLayout Dimension ComponentOrientation]
	   [java.awt.print PrinterJob Printable PrinterException]
	   [java.awt.event ActionListener ComponentListener])
  (:use [clojure.contrib.def]
	[memoverse.utils]
	[memoverse.db])
  (:require [clojure.contrib.seq-utils :as seq]
	    [clojure.contrib.math :as math]))

(defvar *cols-per-page* 2
  "Number of columns per page")
(defvar *rows-per-page* 6
  "Number of rows per page")

(defn- font-fits-in-label?
  "True if font fits in the label"
  [font label]
  (let [font-metrics (.getFontMetrics label font)
	string-width (.stringWidth font-metrics (.getText label))
	line-height (.getHeight font-metrics)
	dimension (.getSize label)
	width (.getWidth dimension)
	height (.getHeight dimension)]
    ;; compare, leaving some tolerance
    (< (* line-height (/ string-width (* 0.8 width))) (* 0.8 height))))

(defn- auto-resize-label
  "Extends JLabel with auto-resize feature"
  [text]
  (proxy [JLabel] [text JLabel/CENTER]
    (paint 
     [g]
     (let [current-font (.getFont this)
	   fonts (map #(.deriveFont current-font (float %))
		      (range 48 8 -1))]
       (loop [my-fonts fonts]
	 (cond
	   ;; exhausted all fonts. use smallest available.
	   (empty? my-fonts)
	   (.setFont this (.deriveFont current-font (float 4)))
	   ;; found a fitting font, use it
	   (font-fits-in-label? (first my-fonts) this)
	   (.setFont this (first my-fonts))
	   ;; try next smaller font
	   :else
	   (recur (rest my-fonts))))
       (proxy-super paint g)))))

(defn- in2px
  "Convert inch to pixels, assuming 72dpi"
  [inch]
  (* 72 inch))

(defn- make-page-n
  "Given a list of verses, return a JFrame representing the page N.
   n is zero indexed.
   side is either :front or :back
   width/height is the dimension of the page"
  [verses n side width height]
  (assert (or (= side :front) (= side :back)))
  (let [total-verses (count verses)
	verses-per-page (* *cols-per-page* *rows-per-page*)
	verses-in-page (if (> verses-per-page total-verses)
			 verses
			 (nth (seq/partition-all verses-per-page verses) n))
	panel (JPanel. (GridLayout. *rows-per-page* *cols-per-page*))
	border (if (= side :front)
		 (BorderFactory/createLineBorder Color/GRAY)
		 (BorderFactory/createCompoundBorder
		  (BorderFactory/createLineBorder Color/GRAY)
		  (BorderFactory/createEmptyBorder (in2px -0.25)   ; top
						   (in2px 0.25)    ; left 
						   (in2px 0.50)    ; bottom
						   (in2px 0.25)))) ; right
	card-dimension (Dimension. (math/floor (/ width *cols-per-page*))
				   (math/floor (/ height *rows-per-page*)))
	page-dimension (Dimension. width height)]
    (doto panel
      (.setBackground Color/WHITE)
      (.setComponentOrientation (if (= side :front)  ; account for page flipping
				  ComponentOrientation/LEFT_TO_RIGHT
				  ComponentOrientation/RIGHT_TO_LEFT)))
    (doseq [[reference category _ content] 
	    ;; need to pad verse-in-page with empty verses because GridLayout
	    ;; will be too smart and place cards in a single column
	    (concat verses-in-page
		    (repeat (- verses-per-page (count verses-in-page))
			    (repeat 4 :empty)))]
      (.add panel (doto (auto-resize-label (if (= reference :empty)
					     ""
					     (format "<html><center>%s</center></html>"
						     (if (= side :front)
						       (format "%s<br>(%s)" reference category)
						       content))))
		    (.setBorder border)
		    (.setVerticalTextPosition JLabel/CENTER)
		    (.setPreferredSize card-dimension)
		    (.setMinimumSize card-dimension)
		    (.setMaximumSize card-dimension))))
    (doto (JFrame. "")
      (.setDefaultCloseOperation WindowConstants/DISPOSE_ON_CLOSE)
      (.add panel)
      (.setBackground Color/WHITE)
      (.pack)
      (.setSize page-dimension)
      (.setResizable false)
      (.setVisible true))))

(defn- make-cards
  "Given a DB, return a Printable object representing the verses arranged in cards"
  [db]
  (let [verses (verses db)
	total-verses (count verses)
	total-pages (* 2 
		       (math/ceil (/ total-verses
				     (* *rows-per-page* 
					*cols-per-page*))))] ; x2 because of front/back matters
    (proxy [Printable] []
      (print
       [graphics page-format page-idx]
       (if (>= page-idx total-pages)
	 Printable/NO_SUCH_PAGE
	 (let [x (.getImageableX page-format)
	       y (.getImageableY page-format)
	       width (.getImageableWidth page-format)
	       height (.getImageableHeight page-format)
	       page (make-page-n verses 
				 (quot page-idx 2) 
				 (if (even? page-idx) :front :back) 
				 width
				 height)]
	   (.translate graphics x y)
	   (.print page graphics)
	   (.dispose page)
	   Printable/PAGE_EXISTS))))))

(defn print-cards
  "Given a DB, print the verses as cards"
  [db]
  (let [print-job (PrinterJob/getPrinterJob)]
    (.setPrintable print-job (make-cards db))
    (when (.printDialog print-job)
      (try (.print print-job)
	   (catch PrinterException e (println "Error printing: " e))))))


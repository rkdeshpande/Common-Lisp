;;;; Application:        Common Lisp Spam Filter
;;;; Written by:         Rohan Deshpande
;;;; Based on code by:   Peter Seibel
;;;; Date:               March 22, 2017

;;; COM.GIGAMONKEYS.SPAM defined in packages.lisp
(in-package :com.gigamonkeys.spam)

;;  Database of features we have statistics on
(defvar *feature-database* (make-hash-table :test #'equal))
(defvar *total-spams* 0)                ; Store total number of spam messages
(defvar *total-hams* 0)                 ; Store total number of ham messages
(defparameter *max-ham-score* .4)       ; Maximum probability allowed for good messages
(defparameter *min-spam-score* .6)      ; Minimum probability needed for bad messages

;;; Main function for this application
(defun classify (text)
  (classification (score (extract-features text))))


;;; Function used by classify to determine what the passed message is
(defun classification (score)
  (values
   (cond
     ((<= score *max-ham-score*) 'ham)
     ((>= score *min-spam-score*) 'spam)
     (t 'unsure))
   score))


;;; This class will track the spam count and ham count for a given word
(defclass word-feature ()
  ((word
    :initarg :word
    :accessor word
    :initform (error "Must supply :word")
    :documentation "The word this feature represents.")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "Number of spams we have seen this feature in.")
   (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "Number of hams we have seen this feature in.")))


(defun clear-database ()
  (setf
   *feature-database* (make-hash-table :test #'equal)
   *total-spams* 0
   *total-hams* 0))


;;; Take a word and return the appropriate feature, creating it if necessary
(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
	    (make-instance 'word-feature :word word))))


;;; Use the Common Lisp Portable Perl-Compatible Regular Expression library
;;; to extract words as features
(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))


;;; Combine extract-words and intern-feature
(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))


;;; Print object representations that include all necessary info
(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))


;;; Basic training function to take text and a symbol and increment the right counts
(defun train (text type)
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))


;;; Take in a word-feature and a message type, and increment the appropriate slot
(defun increment-count (feature type)
  (ecase type
    (ham (incf (ham-count feature)))
    (spam (incf (spam-count feature)))))


;;; Function to increment the overall counts of spam and ham
(defun increment-total-count (type)
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))


;;; Compute basic probability to be used by bayesian probability
(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
	  (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))


;;; This function calculates the per-feature probability of spam
(defun bayesian-spam-probability (feature &optional
					    (assumed-probability 1/2)
					    (weight 1))
  (let ((basic-probability (spam-probability feature))
	(data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
	  (* data-points basic-probability))
       (+ weight data-points))))


;;; Score function to calculate total probability for a message
(defun score (features)
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
	(let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
	  (push spam-prob spam-probs)
	  (push (- 1.0d0 spam-prob) ham-probs)
	  (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
	  (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))

(defun fisher (probs number-of-probs)
  "The Fisher computation described by Robinson."
  (inverse-chi-square
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))

(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min
   (loop with m = (/ value 2)
      for i below (/ degrees-of-freedom 2)
      for prob = (exp (- m)) then (* prob (/ m i))
      summing prob)
   1.0))

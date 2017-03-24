(defvar *account-numbers* 0)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name.")
    :accessor customer-name
    :documentation "Customer's name")
   (balance
    :initarg :balance
    :initform 0
    :reader balance
    :documentation "Current account balance")
   (account-number
    :initform (incf *account-numbers*)
    :reader account-number
    :documentation "Account number, unique within a bank.")
   (account-type
    :reader account-type
    :documentation "Type of account, one of :gold, :silver, or :bronze.")))


(defmethod initialize-instance :after ((account bank-account)
				       &key opening-bonus-percentage)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
	  (cond
	    ((>= balance 100000) :gold)
	    ((>= balance 50000) :silver)
	    (t :bronze))))
  (when opening-bonus-percentage
    (incf (slot-value account 'balance)
	  (* (slot-value account 'balance) (/ opening-bonus-percentage 100)))))

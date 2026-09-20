(defclass propagator ()
  ((name      :initarg :name
              :initform (error "NAME must be provided")
              :accessor name
              :type symbol)
   (value     :initarg :value
              :initform nil
              :accessor value)
   (neighbors :initarg :neighbors
              :initform #()
              :accessor neighbors)))

(defun make-propagator (name &optional (value nil))
  (make-instance 'propagator :name name :value value))

(defun constraint ())

(defclass propagator () ())

(defgeneric alert (this))

(defclass all-different-propagator (propagator)
  ((cells :initarg :cells
          :accessor cells)))

(defun make-all-different (cells)
  (let ((p (make-instance 'all-different-propagator :cells cells)))
    (dolist (c cells)
      (add-propagator c p))
    (alert p)
    p))

(defmethod alert ((p all-different-propagator))
  (dolist (c (cells p))
    (when (= 1 (length (domain c)))
      (dolist (other-cell (cells p))
        (unless (eq c other-cell)
          (let* ((value (first (domain c)))
                 (new-domain (remove value (domain other-cell))))
            (update other-cell new-domain)))))))


(defclass cell ()
  ((name        :initarg :name
                :initform (error "NAME must be provided")
                :accessor name
                :type symbol)
   (domain      :initarg :domain
                :initform (error "DOMAIN must be provided")
                :accessor domain)
   (propagators :initarg :propagators
                :initform '()
                :accessor propagators)))

(defun make-cell (name domain)
  (make-instance 'cell :name name
                       :domain domain))

(defmethod add-propagator ((c cell) (p propagator))
  (unless (member p (propagators c))
    (push p (propagators c))))

(defmethod update ((c cell) new-domain)
  (let ((intersection (intersection (domain c) new-domain)))
    (when (null intersection)
      (error "Contradiction in cell ~a: empty domain!" (name c)))
    (when (< (length intersection) (length (domain c)))
      (setf (domain c) intersection)
      (dolist (p (propagators c))
        (alert p)))))


(defun get-sudoku-dimensions (sudoku-cells)
  (let ((dimension (log (length sudoku-cells) 2)))
    (multiple-value-bind (integer remainder) (truncate dimension)
      (unless (zerop remainder)
        (error "Invalid number of dimensions in SUDOKU-CELLS."))
      integer)))


(defun get-row-cells (n sudoku-cells)
  (let ((dimension (get-sudoku-dimensions sudoku-cells)))
    (loop for i to (1- dimension)
          for cell = (nth (+ i (* n dimension)) sudoku-cells)
          collect cell)))

(defun get-column-cells (n sudoku-cells)
  (let ((dimension (get-sudoku-dimensions sudoku-cells)))
    (loop for i to (1- dimension)
          for cell = (nth (+ n (* i dimension)) sudoku-cells)
          collect cell)))

(defun get-block-cells (n sudoku-cells)
  (let* ((dimension (get-sudoku-dimensions sudoku-cells))
         (blocks-per-row (truncate (log dimension 2))))
    (multiple-value-bind (row-start col-start) (truncate n blocks-per-row)
      (let ((start-index (+ (* col-start blocks-per-row) (* row-start (* dimension blocks-per-row)))))
        (loop for j to (1- blocks-per-row)
              append (loop for i to (1- blocks-per-row)
                           for cell = (nth (+ start-index i (* dimension j)) sudoku-cells)
                           collect cell))))))




(get-block-cells 2 '(0 1 2 3
                     4 5 6 7
                     8 9 10 11
                     12 13 14 15))


(defun sudoku ()
  (let* ((sudoku-grid '(1 0 0 0
                        0 4 0 2
                        4 0 2 0
                        0 0 0 1))
         (sudoku-cells (loop for i in sudoku-grid
                             for cell = (make-cell (intern (format nil "cell-~a" n))
                                                   (if (plusp n)
                                                       (list n)
                                                       '(1 2 3 4)))
                             collect cell)))
    (make-all-different sudoku-cells)))





;;    1 . | . .
;;    . 4 | . 2
;;    ----+----
;;    4 . | 2 .
;;    . . | . 1


;; '(1)   '(2 3) '(2 3 4) '(3 4)
;; '(3)   '(4)   '(1)     '(2)

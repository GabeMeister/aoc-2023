(require :uiop)
(ql:quickload :split-sequence)

(defun trim-whitespace (text)
  (string-trim " " text))

(defun split (char str-to-split)
  (mapcar #'trim-whitespace
      (remove ""
          (remove nil
              (split-sequence:split-sequence char str-to-split)))))

(defun println (object)
  "Print a string but with a newline"
  (format t "~%~a~%" object))

(defun push-to-end (lst item)
  (nconc lst (list item)))

(defun char-to-int (c)
  (- (char-code c) 48))

(defun create-hash ()
  (make-hash-table :test 'equal))

(defun update-hash (hash key value)
  (setf (gethash key hash) value))

(defun delete-hash (hash key)
  (remhash key hash))

(defun get-hash (hash key)
  (gethash key hash))

(defun print-hash-entry (key value)
  (format t "~a, ~a~%" key value))

(defun print-hash (hash)
  (format t "~%=====~%")
  (maphash #'print-hash-entry hash)
  (format t "~%=====~%"))

(defun is-num (c)
  (if (digit-char-p c) t nil))

(defun is-dot (c)
  (equal c #\.))

(defun is-symbol (c)
  (if (and (not (is-num c))
           (not (is-dot c)))
      t
      nil))

(defun is-asterisk (c)
  (if (equal c #\*)
      t
      nil))

(defparameter small-text (uiop:read-file-string "./day4/input-small.txt"))
(defparameter big-text (uiop:read-file-string "./day4/input-big.txt"))

(defparameter lines (split #\newline small-text))


;;
;; CURRENT DAY
;;

(format t "~%=== lines ===~%~a~%" lines)
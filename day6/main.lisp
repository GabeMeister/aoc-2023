(require :uiop)
(ql:quickload :split-sequence)
(ql:quickload :alexandria)

(defun string-includes-p (needle haystack)
  (not (null (search needle haystack))))

(defun trim-whitespace (text)
  (string-trim " " text))

(defun is-empty-str (str)
  (or (equal str "")
      (equal str nil)))

(defun split (char-to-split-on str-to-split)
  (mapcar #'trim-whitespace
      (remove-if #'is-empty-str (split-sequence:split-sequence char-to-split-on str-to-split))))

(defun println (object)
  "Print a string but with a newline"
  (format t "~%~a~%" object))

(defun char-to-int (c)
  (- (char-code c) 48))

;;
;; LISZT (lists)
;;

(defstruct liszt
  (last nil)
  (elements nil))

(defun liszt-push-back (l item)
  (let ((new-item (cons item nil)))
    (if (null (liszt-elements l))
        (progn
         ;; empty list case
         (setf (liszt-elements l) new-item)
         (setf (liszt-last l) new-item))
        (progn
         ;; non-empty list case
         (setf (cdr (liszt-last l)) new-item)
         (setf (liszt-last l) new-item)))))

(defun liszt-push-front (l item)
  (push item (liszt-elements l)))

(defun liszt-pop-front (l)
  ;; If there's only one more in the list of elements, then the last element
  ;; should be nil
  (if (= 1 (length (liszt-elements l)))
      (setf (liszt-last l) nil))

  (pop (liszt-elements l)))

;;
;; HASHES
;;

(defun create-hash ()
  (make-hash-table :test 'equal))

(defun update-hash (hash key value)
  (setf (gethash key hash) value))

(defun delete-hash (hash key)
  (remhash key hash))

(defun clear-hash (hash)
  (clrhash hash))

(defun check-hash (hash key)
  (if (not (equal (get-hash hash key) nil))
      t
      nil))

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

(defparameter small-text (uiop:read-file-string "./day6/input-small.txt"))
(defparameter big-text (uiop:read-file-string "./day6/input-big.txt"))

(defparameter lines (split #\newline big-text))

(println "---------------------")

;;
;; CURRENT DAY
;;

(defun get-times-from-line (line)
  (let ((tokens nil))
    (setf tokens (split #\space line))
    (setf tokens (remove-if (lambda (token) (string= token "Time:")) tokens))
    (setf tokens (mapcar (lambda (token) (parse-integer token)) tokens))

    tokens))

(defun get-distance-from-line (line)
  (let ((tokens nil))
    (setf tokens (split #\space line))
    (setf tokens (remove-if (lambda (token) (string= token "Distance:")) tokens))
    (setf tokens (mapcar (lambda (token) (parse-integer token)) tokens))

    tokens))

(defun get-num-winning-holds (sec record-distance)
  (let ((num-winning-holds 0))
    (loop for s from 1 to (- sec 1) do
            ;; 1
            ;; (sec - s) * s
            ;; (7 - 1) * 1
            ;; (7 - 2) * 2
            (if (> (* s (- sec s)) record-distance)
                (incf num-winning-holds)))

    num-winning-holds))

(defparameter *times-list* nil)
(defparameter *distance-list* nil)

(setf *times-list* (get-times-from-line (pop lines)))
(format t "~%=== *times-list* ===~%~a~%" *times-list*)
(setf *distance-list* (get-distance-from-line (pop lines)))
(format t "~%=== *distance-list* ===~%~a~%" *distance-list*)

(defparameter *winning-holds-list* nil)

(loop for race-time in *times-list*
      for record-distance in *distance-list* do
        (let ((winning-holds (get-num-winning-holds race-time record-distance)))
          (format t "Race Time: ~a sec | Record Distance: ~a~%" race-time record-distance)
          (format t "The number of winning holds: ~a~%" winning-holds)
          (push winning-holds *winning-holds-list*)))

(format t "~%=== *winning-holds-list* ===~%~a~%" *winning-holds-list*)

(defparameter *total-winning-holds* (reduce (lambda (x y) (* x y)) *winning-holds-list*))

(format t "~%=== *total-winning-holds* ===~%~a~%" *total-winning-holds*)

(println "BEFORE")
(println (get-num-winning-holds 52947594 426137412791216))
(println "AFTER")
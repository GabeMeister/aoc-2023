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

(defparameter small-text (uiop:read-file-string "./day8/input-small.txt"))
(defparameter big-text (uiop:read-file-string "./day8/input-big.txt"))

(defparameter lines (split #\newline big-text))

(println "---------------------")

;;
;; CURRENT DAY
;;

(defstruct fork
  (left nil)
  (right nil))

(defparameter *map* (create-hash))
(print-hash *map*)

(defun parse-fork (str)
  (let ()))

(defun parse-direction-mapping-line (line)
  (let ((tokens (split #\= line)))
    (update-hash *map* (nth 0 tokens) (nth 1 tokens))))

(defparameter *directions-str* (pop lines))
(defparameter *directions-list* (coerce *directions-str* 'list))

; (format t "~%=== *directions-list* ===~%~a~%" *directions-list*)

; (dolist (c *directions-list*)
;   (println c))

; (dolist (line lines)
;   (println (parse-direction-mapping-line line)))

; (print-hash *map*)

(defun replace-str (orig str replacement)
  (let ((str-list (coerce orig 'list))
        (final (make-liszt)))
    (println orig)
    (println str)
    (println replacement)

    (dolist (c str-list)
      (if (not (equal c str))
          (liszt-push-back final c)
          (liszt-push-back final replacement)))
    (println final)

    (concatenate 'string str-list)))

(println (replace-str "dog cat zebr a" #\space ""))
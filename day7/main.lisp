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

(defparameter small-text (uiop:read-file-string "./day7/input-small.txt"))
(defparameter big-text (uiop:read-file-string "./day7/input-big.txt"))

(defparameter lines (split #\newline small-text))

(println "---------------------")

;;
;; CURRENT DAY
;;

(dolist (line lines)
  (println line))


; Five of a kind, where all five cards have the same label: AAAAA

; Four of a kind, where four cards have the same label and one card has a different label: AA8AA

; Full house, where three cards have the same label, and the remaining two cards share a different label: 23332

; Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98

; Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432

; One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4

; High card, where all cards' labels are distinct: 23456
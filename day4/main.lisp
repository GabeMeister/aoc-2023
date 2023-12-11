(require :uiop)
(ql:quickload :split-sequence)

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

(defun make-vector ()
  (make-array 0 :adjustable t :fill-pointer 0))

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
(defparameter playground-text (uiop:read-file-string "./day4/input-playground.txt"))

(defparameter lines (split #\newline big-text))

(println "---------------------")

;;
;; CURRENT DAY
;;

(defun get-card-number (line)
  ;; Pass in "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
  ;; -
  ;; Returns `1` as an integer
  (let ((big-chunks (list))
        (small-chunks (list)))
    (setf big-chunks (split #\: line))
    (setf small-chunks (split #\space (nth 0 big-chunks)))
    (parse-integer (nth 1 small-chunks))))

(defun get-winning-numbers (line)
  (let ((big-chunks (list))
        (small-chunks (list)))
    (setf big-chunks (split #\: line))
    (setf small-chunks (split #\| (nth 1 big-chunks)))

    (setf small-chunks (remove-if #'is-empty-str small-chunks))

    (split #\Space (nth 0 (remove-if #'is-empty-str small-chunks)))))

(defun get-user-numbers (line)
  (let ((big-chunks (list))
        (small-chunks (list)))
    (setf big-chunks (split #\: line))
    (setf small-chunks (split #\| (nth 1 (remove-if #'is-empty-str big-chunks))))

    (split #\Space (nth 1 (remove-if #'is-empty-str small-chunks)))))

;; 0 numbers -> 0
;; 1 number -> 1
;; 2 numbers -> 2
;; 3 numbers -> 4
;; 4 numbers -> 8
(defun get-point-amount (winning-num-count)
  (if (equal winning-num-count 0)
      0
      (expt 2 (- winning-num-count 1))))

(defun increment-game-count (hash key)
  (if (get-hash hash key)
      (update-hash hash key (+ 1 (get-hash hash key)))
      (update-hash hash key 1)))

(defparameter *overall-total* 0)
(defparameter *h-game-to-pts* (create-hash))
(defparameter *l-games* (make-liszt))

(loop for i from 0 to (- (length lines) 1) do
        (let ((s-line (nth i lines))
              (i-game-num -1)
              (l-current-winning-nums (list))
              (l-current-user-nums (list))
              (h-winning-nums (create-hash))
              (i-current-total 0)
              (i-winning-num-count 0))
          (setf i-game-num (get-card-number s-line))

          (setf l-current-winning-nums (get-winning-numbers s-line))
          (dolist (s-winning-num l-current-winning-nums)
            (update-hash h-winning-nums s-winning-num t))

          (setf l-current-user-nums (get-user-numbers s-line))

          (dolist (s-user-num l-current-user-nums)
            (if (check-hash h-winning-nums s-user-num)
                (progn
                 (setf i-winning-num-count (+ 1 i-winning-num-count)))))

          (update-hash *h-game-to-pts* i-game-num i-winning-num-count)
          (liszt-push-back *l-games* i-game-num)

          (setf i-current-total (get-point-amount i-winning-num-count))
          (setf *overall-total* (+ *overall-total* i-current-total))))

; (defparameter *total-games* 0)

; (loop
;  (if (<= (length (liszt-elements *l-games*)) 0) (return))

;  (format t "~%Length:~%~a~%" (length (liszt-elements *l-games*)))

;  ;; Pop the first game off and count it
;  (let ((i-current-game (liszt-pop-front *l-games*)))
;    (setf *total-games* (+ 1 *total-games*))

;    (let ((i-additional (get-hash *h-game-to-pts* i-current-game))
;          (i-next-game-start-idx (+ i-current-game 1)))
;      (loop for i-addl-idx from i-next-game-start-idx to (+ i-current-game i-additional)
;            do (progn
;                (liszt-push-back *l-games* i-addl-idx))))))

; (format t "~%=== *total-games* ===~%~a~%" *total-games*)


(defparameter *h-game-to-copies* (create-hash))
(defun calculate-copies (i-game i-pts)
  (let ((current-game-copies nil))

    ;; Add just the original game itself
    (if (not (get-hash *h-game-to-copies* i-game))
        (update-hash *h-game-to-copies* i-game 1)
        (update-hash *h-game-to-copies* i-game (+ 1 (get-hash *h-game-to-copies* i-game))))

    (setf current-game-copies (get-hash *h-game-to-copies* i-game))

    ;; Add the copies
    (loop for i from (+ i-game 1) to (+ i-game i-pts)
          do
            (if (not (get-hash *h-game-to-copies* i))
                (update-hash *h-game-to-copies* i current-game-copies)
                (update-hash *h-game-to-copies* i (+ (get-hash *h-game-to-copies* i) current-game-copies))))))

(maphash #'calculate-copies *h-game-to-pts*)

(print-hash *h-game-to-copies*)
(defparameter *total-games* 0)
(maphash (lambda (key val) (progn (format nil "~a~%" key) (setf *total-games* (+ *total-games* val)))) *h-game-to-copies*)

(format t "Total games: ~a~%" *total-games*)
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

(defparameter small-text (uiop:read-file-string "./day3/small.txt"))
(defparameter big-text (uiop:read-file-string "./day3/big.txt"))
(defparameter another-text (uiop:read-file-string "./day3/another.txt"))

(defparameter lines (split #\newline big-text))
(defparameter *asterisk-map* (create-hash))

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

(defun get-number-value (the-number)
  (let ((num-str ""))
    (dolist (curr-digit (num-digits the-number))
      (setq num-str (format nil "~a~a" num-str (digit-value curr-digit))))

    (parse-integer num-str)))

(defun get-val (row col arr)
  (if (or (< row 0)
          (>= row (length arr))
          (< col 0)
          (>= col (length arr)))
      #\.
      (nth col (nth row arr))))

(defstruct digit
  (value 0)
  (row 0)
  (col 0))

(defstruct num
  (digits '()))

(defun update-2d-list (lst row col value)
  (setf (nth col (nth row lst)) value))

(defun get-adjacent-asterisk-location (number-to-check grid)
  (dolist (curr-digit (num-digits number-to-check))
    (let ((row-idx (digit-row curr-digit))
          (col-idx (digit-col curr-digit)))

      ;; top-left
      (if (is-asterisk (get-val (- row-idx 1) (- col-idx 1) grid))
          (return-from get-adjacent-asterisk-location (format nil "~a_~a" (- row-idx 1) (- col-idx 1))))
      ;; top
      (if (is-asterisk (get-val (- row-idx 1) col-idx grid))
          (return-from get-adjacent-asterisk-location (format nil "~a_~a" (- row-idx 1) col-idx)))
      ;; top-right
      (if (is-asterisk (get-val (- row-idx 1) (+ col-idx 1) grid))
          (return-from get-adjacent-asterisk-location (format nil "~a_~a" (- row-idx 1) (+ col-idx 1))))
      ;; left
      (if (is-asterisk (get-val row-idx (- col-idx 1) grid))
          (return-from get-adjacent-asterisk-location (format nil "~a_~a" row-idx (- col-idx 1))))
      ;; right
      (if (is-asterisk (get-val row-idx (+ col-idx 1) grid))
          (return-from get-adjacent-asterisk-location (format nil "~a_~a" row-idx (+ col-idx 1))))
      ;; bottom-left
      (if (is-asterisk (get-val (+ row-idx 1) (- col-idx 1) grid))
          (return-from get-adjacent-asterisk-location (format nil "~a_~a" (+ row-idx 1) (- col-idx 1))))
      ;; bottom
      (if (is-asterisk (get-val (+ row-idx 1) col-idx grid))
          (return-from get-adjacent-asterisk-location (format nil "~a_~a" (+ row-idx 1) col-idx)))
      ;; bottom-right
      (if (is-asterisk (get-val (+ row-idx 1) (+ col-idx 1) grid))
          (return-from get-adjacent-asterisk-location (format nil "~a_~a" (+ row-idx 1) (+ col-idx 1)))))))

(defun is-num-adjacent-to-asterisk (number-to-check grid)
  (let ((result nil))
    (dolist (curr-digit (num-digits number-to-check))
      (let ((row-idx (digit-row curr-digit))
            (col-idx (digit-col curr-digit)))

        ;; top-left
        (if (is-asterisk (get-val (- row-idx 1) (- col-idx 1) grid))
            (return-from is-num-adjacent-to-asterisk t))
        ;; top
        (if (is-asterisk (get-val (- row-idx 1) col-idx grid))
            (return-from is-num-adjacent-to-asterisk t))
        ;; top-right
        (if (is-asterisk (get-val (- row-idx 1) (+ col-idx 1) grid))
            (return-from is-num-adjacent-to-asterisk t))
        ;; left
        (if (is-asterisk (get-val row-idx (- col-idx 1) grid))
            (return-from is-num-adjacent-to-asterisk t))
        ;; right
        (if (is-asterisk (get-val row-idx (+ col-idx 1) grid))
            (return-from is-num-adjacent-to-asterisk t))
        ;; bottom-left
        (if (is-asterisk (get-val (+ row-idx 1) (- col-idx 1) grid))
            (return-from is-num-adjacent-to-asterisk t))
        ;; bottom
        (if (is-asterisk (get-val (+ row-idx 1) col-idx grid))
            (return-from is-num-adjacent-to-asterisk t))
        ;; bottom-right
        (if (is-asterisk (get-val (+ row-idx 1) (+ col-idx 1) grid))
            (return-from is-num-adjacent-to-asterisk t))))
    result))

(defun is-num-adjacent-to-symbol (number-to-check grid)
  (let ((result nil))
    (dolist (curr-digit (num-digits number-to-check))
      (let ((row-idx (digit-row curr-digit))
            (col-idx (digit-col curr-digit)))

        ;; top-left
        (if (is-symbol (get-val (- row-idx 1) (- col-idx 1) grid))
            (return-from is-num-adjacent-to-symbol t))
        ;; top
        (if (is-symbol (get-val (- row-idx 1) col-idx grid))
            (return-from is-num-adjacent-to-symbol t))
        ;; top-right
        (if (is-symbol (get-val (- row-idx 1) (+ col-idx 1) grid))
            (return-from is-num-adjacent-to-symbol t))
        ;; left
        (if (is-symbol (get-val row-idx (- col-idx 1) grid))
            (return-from is-num-adjacent-to-symbol t))
        ;; right
        (if (is-symbol (get-val row-idx (+ col-idx 1) grid))
            (return-from is-num-adjacent-to-symbol t))
        ;; bottom-left
        (if (is-symbol (get-val (+ row-idx 1) (- col-idx 1) grid))
            (return-from is-num-adjacent-to-symbol t))
        ;; bottom
        (if (is-symbol (get-val (+ row-idx 1) col-idx grid))
            (return-from is-num-adjacent-to-symbol t))
        ;; bottom-right
        (if (is-symbol (get-val (+ row-idx 1) (+ col-idx 1) grid))
            (return-from is-num-adjacent-to-symbol t))))
    result))

(defparameter *grid* '())

(dolist (line lines)
  (push (coerce line 'list) *grid*))

(setq *grid* (reverse *grid*))

(defparameter *num-rows* (length *grid*))
(defparameter *num-cols* (length (nth 0 *grid*)))
(defparameter *last-row-idx* (- *num-rows* 1))
(defparameter *last-col-idx* (- *num-cols* 1))

(defparameter *parsing-num* nil)
(defparameter *current-num* nil)
(defparameter *current-digit* nil)
(defparameter *all-nums* nil)

(defparameter *current-char* ".")

(loop for r from 0 to *last-row-idx*
      do (progn
          (loop for c from 0 to *last-col-idx*
                do
                  (setf *current-char* (get-val r c *grid*))
                  (if (is-num *current-char*)
                      (progn
                       (setf *current-digit* (make-digit
                                               :value (char-to-int *current-char*)
                                               :row r
                                               :col c))

                       (if (equal *parsing-num* t)
                           (progn
                            (setf (num-digits *current-num*) (append (num-digits *current-num*) (list *current-digit*))))
                           (progn
                            (setf *current-num* (make-num
                                                  :digits (list *current-digit*)))
                            (setf *all-nums* (append *all-nums* (list *current-num*)))))

                       (setf *parsing-num* t))
                      (progn
                       (setf *parsing-num* nil)
                       (if (is-asterisk *current-char*)
                           (update-hash *asterisk-map* (format nil "~a_~a" r c) (list))))))
          (setf *parsing-num* nil)))


; (defparameter total 0)
; (dolist (curr-num *all-nums*)
;   (if (is-num-adjacent-to-symbol curr-num *grid*)
;       (progn
;        (format t "MATCH ~a~%" (get-number-value curr-num))
;        (setq total (+ total (get-number-value curr-num))))
;       (format t "~a does not match!~%" (get-number-value curr-num))))

; (format t "The total is: ~a~%" total)


(defun get-adjacent-asterisk (num grid)
  (format t "~%=== num ===~%~a~%" num))

(dolist (curr-num *all-nums*)
  (if (is-num-adjacent-to-asterisk curr-num *grid*)
      (let ((loc-str (get-adjacent-asterisk-location curr-num *grid*)))
        (update-hash *asterisk-map* loc-str (append (get-hash *asterisk-map* loc-str) (list curr-num))))))


(defparameter *gear-ratio-total* 0)
(defun process-asterisk (key value)
  (if (equal (length value) 2)
      (let ((first-num (get-number-value (nth 0 value)))
            (second-num (get-number-value (nth 1 value))))
        (format t "~%=== first-num ===~%~a~%" first-num)
        (format t "~%=== second-num ===~%~a~%" second-num)
        (format t "~%=== PRODUCT ===~%~a~%" (* first-num second-num))
        (setf *gear-ratio-total* (+ *gear-ratio-total* (* first-num second-num))))
      (format t "Passing over asterisk w/ key: ~a~%" key)))

(maphash #'process-asterisk *asterisk-map*)

(format t "~%=== *gear-ratio-total* ===~%~a~%" *gear-ratio-total*)
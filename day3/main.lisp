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

(defvar small-text "")
(setq small-text (uiop:read-file-string "./day3/small.txt"))
(defvar big-text "")
(setq big-text (uiop:read-file-string "./day3/big.txt"))
(defvar another-text "")
(setq another-text (uiop:read-file-string "./day3/another.txt"))
(defvar lines (list))
(setq lines (split #\newline another-text))

(defun is-num (c)
  (if (digit-char-p c) t nil))

(defun is-dot (c)
  (equal c #\.))

(defun is-symbol (c)
  (if (and (not (is-num c))
           (not (is-dot c)))
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

(defun get-symbol-locations ())

(defun get-adjacent-symbol-locations ())

(defstruct digit
  (value 0)
  (row 0)
  (col 0))

(defstruct num
  (digits '()))

(defun update-2d-list (lst row col value)
  (setf (nth col (nth row lst)) value))

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

(defvar grid)
(setf grid '())

(dolist (line lines)
  (push (coerce line 'list) grid))

(setq grid (reverse grid))

(defvar num-rows)
(setq num-rows (length grid))
(defvar num-cols)
(setq num-cols (length (nth 0 grid)))
(defvar last-row-idx)
(setq last-row-idx (- num-rows 1))
(defvar last-col-idx)
(setq last-col-idx (- num-cols 1))

(defvar parsing-num)
(setf parsing-num nil)
(defvar current-num)
(setf current-num nil)
(defvar current-digit)
(setf current-digit nil)
(defvar all-nums)
(setf all-nums nil)

(defvar current-char)
(setf current-char ".")

(loop for r from 0 to last-row-idx
      do (loop for c from 0 to last-col-idx
               do
                 (setf current-char (get-val r c grid))
                 (if (is-num current-char)
                     (progn
                      (setf current-digit (make-digit :value (char-to-int current-char) :row r :col c))

                      (if (equal parsing-num t)
                          (progn
                           (setf (num-digits current-num) (append (num-digits current-num) (list current-digit))))
                          (progn
                           (setf current-num (make-num :digits (list current-digit)))
                           (setf all-nums (append all-nums (list current-num)))))

                      (setf parsing-num t))
                     (progn
                      (setf parsing-num nil)))))

(defvar total)
(setq total 0)
(dolist (curr-num all-nums)
  (if (is-num-adjacent-to-symbol curr-num grid)
      (progn
       (format t "MATCH ~a~%" (get-number-value curr-num))
       (setq total (+ total (get-number-value curr-num))))
      (format t "~a does not match!~%" (get-number-value curr-num))))

(format t "The total is: ~a~%" total)


; Too high: 1837615
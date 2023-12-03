(require :uiop)
(ql:quickload :split-sequence)

(defun trim-whitespace (text)
  (string-trim " " text)
)

(defun split (char str-to-split)
  (mapcar #'trim-whitespace
	  (remove ""
		  (remove nil
			  (split-sequence:split-sequence char str-to-split)))))

(defun println (object)
  "Print a string but with a newline"
  (format t "~%~a~%" object))

(defvar big-text (uiop:read-file-string "./big.txt"))
(defvar lines (remove "" (split #\Newline big-text) :test #'equal))

(defun get-game-number (line)
  (let ((tokens (split #\: line)))
    (let ((game-number (split #\Space (first tokens))))
      (parse-integer (nth 1 game-number))
    )
  )
)

;; "2 blue" -> 2
(defun get-color-count (line)
  (let ((tokens (split #\Space line)))
    (list (parse-integer (first tokens)) (nth 1 tokens))
  )
)

;; Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
(defun get-max-color (color line)
  ;; "Game 1" | "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  (let ((chunks (split #\: line)))
    ;; (format t "GAME ~a ~a" (get-game-number line) color)
    ;; "3 blue, 4 red" | "1 red, 2 green, 6 blue" | "2 green"
    (let ((rounds (split #\; (nth 1 chunks)))
	  (highest 0))
      (dolist (round rounds)
	;; "3 blue" | "4 red"
	(let ((hands (split #\, round)))
	  (dolist (hand hands)
	    (let ((hand-values (get-color-count hand)))
	      (if (and (string= color (nth 1 hand-values))
		       (> (nth 0 hand-values) highest))
		  (setq highest (nth 0 hand-values)))
	    )
	  )
	)
      )

      highest
    )
  )
)

;; 12 red cubes, 13 green cubes, and 14 blue cubes
(defvar red-total 12)
(defvar green-total 13)
(defvar blue-total 14)

(defvar game-total 0)
(setq game-total 0)

(println lines)

(dolist (line lines)
  (let ((game-id (get-game-number line)))
    (if (and (<= (get-max-color "red" line) red-total)
	     (<= (get-max-color "green" line) green-total)
	     (<= (get-max-color "blue" line) blue-total))
	(progn
	  (format t "GAME #~a WORKS~%" game-id)
	  (setq game-total (+ game-total game-id))))))

(println game-total)



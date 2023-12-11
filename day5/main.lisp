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

(defparameter small-text (uiop:read-file-string "./day5/input-small.txt"))
(defparameter big-text (uiop:read-file-string "./day5/input-big.txt"))

(defparameter lines (split #\newline big-text))

(println "---------------------")

;;
;; CURRENT DAY
;;

(defstruct num-range (low 0) (high 0) (diff 0))

(defparameter *l-seeds* nil)
(defparameter *l-seed-to-soil* (make-liszt))
(defparameter *l-soil-to-fertilizer* (make-liszt))
(defparameter *l-fertilizer-to-water* (make-liszt))
(defparameter *l-water-to-light* (make-liszt))
(defparameter *l-light-to-temperature* (make-liszt))
(defparameter *l-temperature-to-humidity* (make-liszt))
(defparameter *l-humidity-to-location* (make-liszt))

(defparameter *s-seed-line* (pop lines))
(defparameter *l-seeds* (split #\space *s-seed-line*))
(setf *l-seeds* (remove-if (lambda (token) (string= "seeds:" token)) *l-seeds*))
(setf *l-seeds* (mapcar (lambda (s) (parse-integer s)) *l-seeds*))


(defparameter *s-category* nil)

(defun create-range-from-line (line)
  (let ((tokens (split #\space line)))
    (make-num-range
      :low (parse-integer (nth 1 tokens))
      :high (+ (parse-integer (nth 1 tokens)) (parse-integer (nth 2 tokens)) -1)
      :diff (- (parse-integer (nth 0 tokens)) (parse-integer (nth 1 tokens))))))

(defun apply-mapping (i-val l-mappings)
  (dolist (mapping (liszt-elements l-mappings))
    (if (and (>= i-val (num-range-low mapping))
             (<= i-val (num-range-high mapping)))
        (return-from apply-mapping (+ i-val (num-range-diff mapping)))))

  i-val)

(dolist (line lines)
  (if (string-includes-p " map:" line)
      (setf *s-category* line)
      (progn
       (alexandria:switch (*s-category* :test #'equal)
         ("seed-to-soil map:"
          (progn
           (let ((new-seed-to-soil (create-range-from-line line)))
             (liszt-push-back *l-seed-to-soil* new-seed-to-soil))))
         ("soil-to-fertilizer map:"
          (let ((new-soil-to-fertilizer (create-range-from-line line)))
            (liszt-push-back *l-soil-to-fertilizer* new-soil-to-fertilizer)))
         ("fertilizer-to-water map:"
          (let ((new-fertilizer-to-water (create-range-from-line line)))
            (liszt-push-back *l-fertilizer-to-water* new-fertilizer-to-water)))
         ("water-to-light map:"
          (let ((new-water-to-light (create-range-from-line line)))
            (liszt-push-back *l-water-to-light* new-water-to-light)))
         ("light-to-temperature map:"
          (let ((new-light-to-temperature (create-range-from-line line)))
            (liszt-push-back *l-light-to-temperature* new-light-to-temperature)))
         ("temperature-to-humidity map:"
          (let ((new-temperature-to-humidity (create-range-from-line line)))
            (liszt-push-back *l-temperature-to-humidity* new-temperature-to-humidity)))
         ("humidity-to-location map:"
          (let ((new-humidity-to-location (create-range-from-line line)))
            (liszt-push-back *l-humidity-to-location* new-humidity-to-location)))))))

(defparameter *l-location-nums* (make-liszt))

(dolist (seed *l-seeds*)
  (let ((i-current seed))
    (println "------------------")
    (setf i-current (apply-mapping i-current *l-seed-to-soil*))
    (format t "Soil: ~a~%" i-current)
    (setf i-current (apply-mapping i-current *l-soil-to-fertilizer*))
    (format t "Fertilizer: ~a~%" i-current)
    (setf i-current (apply-mapping i-current *l-fertilizer-to-water*))
    (format t "Water: ~a~%" i-current)
    (setf i-current (apply-mapping i-current *l-water-to-light*))
    (format t "Light: ~a~%" i-current)
    (setf i-current (apply-mapping i-current *l-light-to-temperature*))
    (format t "Temperature: ~a~%" i-current)
    (setf i-current (apply-mapping i-current *l-temperature-to-humidity*))
    (format t "Humidity: ~a~%" i-current)
    (setf i-current (apply-mapping i-current *l-humidity-to-location*))
    (format t "Location: ~a~%" i-current)

    (liszt-push-back *l-location-nums* i-current)))

; (format t "~%=== *l-location-nums* ===~%~a~%" *l-location-nums*)
(println "-------------")
(format t "The smallest location is: ~a~%" (apply #'min (liszt-elements *l-location-nums*)))
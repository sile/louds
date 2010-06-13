(in-package :louds)

(defconstant +BLOCK-SIZE+ 64)
(defconstant +WORD-SIZE+  32)
(defconstant +SELECT-INDEX-INTERVAL+ 64) ;128)

#-IGNORE
(defparameter *sample-bits*
  (coerce (loop REPEAT 1000000 COLLECT (if (zerop (random 2)) 1 0)) 'bit-vector))

(deftype block-number () '(mod #.(floor array-total-size-limit +BLOCK-SIZE+)))
(deftype array-index () '(mod #.array-total-size-limit))
(deftype positive-fixnum () '(mod #.most-positive-fixnum))
(deftype uint32 () '(unsigned-byte 32))

(defstruct (bitvector (:conc-name ""))
  (blocks                   t :type (simple-array uint32))
  (block-precede-1bit-count t :type (simple-array positive-fixnum))
  (select-indices           t :type (simple-array block-number)))

(defun bits-to-num (bits &optional (start 0) (end (length bits)))
  (loop FOR i      FROM start BELOW (min end (length bits))
	FOR offset FROM 0
	SUM (ash (bit bits i) offset)))

(defun divide-bit-string (bit-string block-size &aux (len (length bit-string)))
  (loop FOR start FROM 0 BELOW len BY block-size
	COLLECT (subseq bit-string start (min (+ start block-size) len)) INTO bits-list
	FINALLY (return (coerce bits-list 'vector))))

(defun make-bit-blocks (bit-string)
  (divide-bit-string bit-string +BLOCK-SIZE+))

(defun make-blocks (bit-blocks)
  (loop FOR bits ACROSS bit-blocks
    APPEND `(,(bits-to-num bits 0 +WORD-SIZE+)
	     ,(bits-to-num bits +WORD-SIZE+ +BLOCK-SIZE+)) INTO block-list
    FINALLY (return (coerce block-list '(vector uint32)))))

(defun make-block-precede-1bit-count (bit-blocks)
  (loop FOR bits ACROSS bit-blocks
	FOR count = (count 1 bits)
	AND total = 0 THEN (+ total count)
    COLLECT total INTO 0bit-counts
    FINALLY (return (coerce (append 0bit-counts `(,total)) '(vector array-index)))))

;;;;;;;;;
;; TODO: 最後にも番兵値を入れる
(defun make-select-indices-list (bit-blocks)
  (loop WITH index = 0
	WITH nth   = 0
	FOR bits ACROSS bit-blocks
    APPEND (loop FOR b ACROSS bits
             WHEN (and (incf index)
		       (= b 1)
		       (incf nth)
		       (zerop (mod nth +SELECT-INDEX-INTERVAL+)))
	     COLLECT (floor (1- index) +BLOCK-SIZE+))
      INTO indices
    FINALLY (return (append (cons 0 indices)
			    `(,(length bit-blocks))))))

(defun make-select-indices (select-indices-list)
  (coerce select-indices-list '(vector block-number)))

(defun build-bitvector (bit-string)
  (let* ((bit-blocks (make-bit-blocks bit-string))
	 (blocks     (make-blocks bit-blocks))
	 (select-indices-list (make-select-indices-list bit-blocks)))
    (make-bitvector
     :select-indices           (make-select-indices select-indices-list)
     
     :blocks                   blocks
     :block-precede-1bit-count (make-block-precede-1bit-count bit-blocks))))

(defparameter *sample-bv* (build-bitvector *sample-bits*))

(declaim (inline get-block))
(declaim (ftype (function (block-number bitvector) (values uint32 uint32)) get-block))
(defun get-block (block-num bitvector)
  (with-slots (blocks) bitvector
    (values (aref blocks (+ 0 (* 2 block-num)))
	    (aref blocks (+ 1 (* 2 block-num))))))

(declaim (inline block-select1)) 
(defun block-select1 (nth block-num bitvector)
  (labels ((impl (nth block beg end)
             (let* ((m (+ beg (floor (- end beg) 2)))
		    (i (logcount (ldb (byte m 0) block))))
	       (declare ((mod 33) m))
	       (cond ((= nth i) (1- (integer-length (ldb (byte m 0) block))))
		     ((< nth i) (impl nth block beg m))
		     (t         (impl nth block m end))))))
    (declare (ftype (function ((mod 65) uint32 (mod 33) (mod 65)) (mod 33)) impl))
    (multiple-value-bind (block-low block-high) (get-block block-num bitvector)
      (let ((i (logcount block-low)))
	(cond ((= nth i) (1- (integer-length block-low)))
	      ((< nth i) (impl nth block-low 0 32))
	      (t   (+ 32 (impl (- nth i) block-high 0 64))))))))

(declaim (inline target-block-bound))
(defun target-block-bound (nth bitvector)
  (with-slots (select-indices) bitvector
    (let ((idx (floor nth +SELECT-INDEX-INTERVAL+)))
      (values (+ 0 (aref select-indices (+ 0 idx)))
	      (+ 1 (aref select-indices (+ 1 idx))))))) ; (values 0 (length ...))

(defun select1 (nth bitvector)
  (declare #.*fastest*
	   (positive-fixnum nth)
	   (bitvector bitvector))
  (with-slots (block-precede-1bit-count) bitvector
    (multiple-value-bind (block-beg block-end) (target-block-bound nth bitvector)
      (loop FOR block-num OF-TYPE block-number = (+ block-beg (floor (- block-end block-beg) 2))
	    FOR start OF-TYPE positive-fixnum  = (aref block-precede-1bit-count block-num)
	    FOR end   OF-TYPE positive-fixnum  = (1+ (aref block-precede-1bit-count (1+ block-num)))
        WHEN (< start nth end) DO (loop-finish)
	WHEN (<= nth start)    DO (setf block-end block-num)
	WHEN (>= nth end)      DO (setf block-beg block-num)
	FINALLY 
	(return (+ (* block-num +BLOCK-SIZE+) (block-select1 (- nth start) block-num bitvector)))))))

(declaim (inline block-rank1))
(defun block-rank1 (offset block-num bitvector)
  (multiple-value-bind (block-low block-high) (get-block block-num bitvector)
    (if (< offset 32)
	(logcount (ldb (byte (1+ offset) 0) block-low))
      (+ (logcount block-low)
	 (logcount (ldb (byte (1+ (- offset 32)) 0) block-high))))))

(defun rank1 (index bitvector)
  (declare #.*fastest*
	   (bitvector bitvector)
	   (array-index index))
  (multiple-value-bind (block-num offset) (floor index +BLOCK-SIZE+)
    (with-slots (block-precede-1bit-count) bitvector
      (let ((pre-1bit-count (aref block-precede-1bit-count block-num)))
	(declare (positive-fixnum pre-1bit-count))
	(the positive-fixnum
	     (+ pre-1bit-count (block-rank1 offset block-num bitvector)))))))

(declaim (inline rank0))
(defun rank0 (index bitvector)
  (declare #.*fastest*
	   (array-index index))
  (- (1+ index) (rank1 index bitvector)))

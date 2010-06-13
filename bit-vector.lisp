(in-package :louds)

(defconstant +BLOCK-SIZE+ 64)
(defconstant +WORD-SIZE+  32)

(defparameter *sample-bits*
  (coerce (loop REPEAT 1000000 COLLECT (if (zerop (random 2)) 1 0)) 'bit-vector))

(deftype uint8  () '(unsigned-byte 8))
(deftype uint16 () '(unsigned-byte 16))
(deftype uint32 () '(unsigned-byte 32))
(deftype block-number () '(mod #.(floor array-total-size-limit +BLOCK-SIZE+)))
(deftype array-index () '(mod #.array-total-size-limit))
(deftype positive-fixnum () '(mod #.most-positive-fixnum))

(defstruct (bitvector (:conc-name ""))
  (blocks                   t :type (simple-array uint32))
  (block-precede-1bit-count t :type (simple-array uint32))
  (block-1bit-count         t :type (simple-array uint8)))        

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
    UNLESS (= +BLOCK-SIZE+ count)
    COLLECT total INTO 0bit-counts
    FINALLY (return (coerce 0bit-counts '(vector uint32)))))

(defun make-block-1bit-count (bit-blocks)
  (loop FOR bits ACROSS bit-blocks 
    COLLECT (count 1 bits) INTO 1bit-counts
    FINALLY (return (coerce 1bit-counts '(vector uint8)))))

(defun build-bitvector (bit-string)
  (let* ((bit-blocks (make-bit-blocks bit-string))
	 (blocks     (make-blocks bit-blocks)))
    (make-bitvector
     :blocks                   blocks
     :block-precede-1bit-count (make-block-precede-1bit-count bit-blocks)
     :block-1bit-count         (make-block-1bit-count bit-blocks))))

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

(defun select1 (nth bitvector)
  (declare #.*fastest*
	   (positive-fixnum nth)
	   (bitvector bitvector))
  (with-slots (block-precede-1bit-count block-1bit-count blocks) bitvector
    (loop WITH block-beg OF-TYPE block-number = 0
	  WITH block-end OF-TYPE block-number = (/ (length blocks) 2)
	  FOR block-num  OF-TYPE block-number = (+ block-beg (floor (- block-end block-beg) 2))
	  FOR start OF-TYPE positive-fixnum = (aref block-precede-1bit-count block-num)
	  FOR end   OF-TYPE positive-fixnum = (+ start (aref block-1bit-count block-num) 1)
      WHEN (< start nth end)
        DO (loop-finish)
      WHEN (<= nth start)
        DO (setf block-end block-num)
      WHEN (>= nth end)
        DO (setf block-beg block-num)
      FINALLY 
      (return (+ (* block-num +BLOCK-SIZE+) (block-select1 (- nth start) block-num bitvector))))))

#|
(defparameter *sample-bits*
  (coerce (loop REPEAT 10000000 COLLECT (if (zerop (random 200)) 1 0)) 'bit-vector))

(defconstant +BLOCK-SIZE+ 64)
(defconstant +WORD-SIZE+  32)
(defconstant +SELECT-INDEX-INTERVAL+ 32)

(deftype uint8  () '(unsigned-byte 8))
(deftype uint16 () '(unsigned-byte 16))
(deftype uint32 () '(unsigned-byte 32))
(deftype block-number () '(mod #.(floor array-total-size-limit +BLOCK-SIZE+)))
(deftype array-index () '(mod #.array-total-size-limit))
(deftype positive-fixnum () '(mod #.most-positive-fixnum))

(defstruct (bitvector (:conc-name ""))
  (blocks                                   t :type (simple-array uint32))
  (block-precede-0bit-count                 t :type (simple-array uint32))
  (block-1bit-count                         t :type (simple-array uint8))
  (block-1bit-count-until-last-select-index t :type (simple-array uint8))
  (select-indices                           t :type (simple-array uint16))
  (src-block-all-0bit-flag                  t :type (simple-array uint32))
  (SBC0F-rank-indices                       t :type (simple-array uint32))) ;; XXX: indicesではない

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

(defun make-block-precede-0bit-count (bit-blocks)
  (loop FOR bits ACROSS bit-blocks
	FOR count = (count 0 bits)
	AND total = 0 THEN (+ total count)
    UNLESS (= +BLOCK-SIZE+ count)
    COLLECT total INTO 0bit-counts
    FINALLY (return (coerce 0bit-counts '(vector uint32)))))

(defun make-block-1bit-count (bit-blocks)
  (loop FOR bits ACROSS bit-blocks 
    COLLECT (count 1 bits) INTO 1bit-counts
    FINALLY (return (coerce 1bit-counts '(vector uint8)))))

(defun make-select-indices-list (bit-blocks)
  (loop WITH index = 0
	WITH nth   = 0
	FOR bits ACROSS bit-blocks
    APPEND (loop FOR b ACROSS bits
             WHEN (and (incf index)
		       (= b 1)
		       (incf nth)
		       (zerop (mod nth +SELECT-INDEX-INTERVAL+)))
	     COLLECT index)
      INTO indices
    FINALLY (return (cons 0 indices))))

(defun make-select-indices (select-indices-list &optional encoded-indices (base 0) (i 0))
  (flet ((low  (n) (ldb (byte 16 0)  n))
	 (high (n) (ldb (byte 16 16) n)))
    (if (endp select-indices-list)
	(coerce (nreverse encoded-indices) '(vector uint16))
      (destructuring-bind (idx . rest) select-indices-list
        (if (zerop i)
	    (make-select-indices rest `(,(high idx) ,(low idx) ,@encoded-indices) idx 7)
	  (make-select-indices rest `(,(- idx base) ,@encoded-indices) base (1- i)))))))

(defun make-block-1bit-count-until-last-select-index (select-indices-list bit-blocks)
  (let ((ary (make-array (length bit-blocks) :element-type 'uint8 :initial-element 0)))
    (loop FOR idx IN select-indices-list
	  FOR block-num = (floor idx +BLOCK-SIZE+)
	  FOR bits      = (aref bit-blocks block-num)
      DO (setf (aref ary block-num)
	       (count 1 bits :end (mod idx +BLOCK-SIZE+))))
    ary))

(defun make-src-block-all-0bit-flag (bit-blocks)
  (let ((flag-bits
	 (loop FOR bits ACROSS bit-blocks
	       COLLECT (if (zerop (count 1 bits)) 1 0) INTO flag-bits
	       FINALLY (return (coerce flag-bits 'bit-vector)))))
    (map '(vector uint32) #'bits-to-num (divide-bit-string flag-bits +WORD-SIZE+))))
  
(defun make-SBC0F-rank-indices (src-block-all-0bit-flag)
  (loop WITH nth = 0
	FOR uint32 ACROSS src-block-all-0bit-flag
    COLLECT (prog1 nth (incf nth (logcount uint32))) INTO rank-indices
    FINALLY (return (coerce rank-indices '(vector uint32)))))

(defun build-bitvector (bit-string)
  (let* ((bit-blocks (make-bit-blocks bit-string))
	 (bit-blocks-no-all0 (remove-if (lambda (bits) (every #'zerop bits)) bit-blocks))
	 (blocks     (make-blocks bit-blocks-no-all0))
	 (select-indices-list (make-select-indices-list bit-blocks-no-all0))
	 (src-block-all-0bit-flag (make-src-block-all-0bit-flag bit-blocks)))
    (make-bitvector
     :blocks                   blocks
     :block-precede-0bit-count (make-block-precede-0bit-count bit-blocks)
     :block-1bit-count         (make-block-1bit-count bit-blocks-no-all0)
     :block-1bit-count-until-last-select-index
       (make-block-1bit-count-until-last-select-index select-indices-list bit-blocks-no-all0)
     :select-indices           (make-select-indices select-indices-list)
     :src-block-all-0bit-flag  src-block-all-0bit-flag
     :SBC0F-rank-indices       (make-SBC0F-rank-indices src-block-all-0bit-flag))))

(defparameter *sample-bv* (build-bitvector *sample-bits*))

;;;;;;;;;;;
;;;; select
(declaim (inline get-lie-block))
(defun get-lie-block (nth bitvector)  ;; XXX: 名前
  (with-slots (select-indices) bitvector
    (let ((base-nth (floor nth +SELECT-INDEX-INTERVAL+)))
      (multiple-value-bind (quot rem) (floor base-nth 8)
        (let ((index (+ (ash (aref select-indices (+ 0 (* quot 9)))  0)
			(ash (aref select-indices (+ 1 (* quot 9)))  16))))
	  (declare (array-index index))
	  (unless (zerop rem)
	    (incf index (the positive-fixnum (aref select-indices (+ base-nth quot 1)))))

	  (values (* base-nth +SELECT-INDEX-INTERVAL+)
		  index))))))

(declaim (inline get-block))
(declaim (ftype (function (block-number bitvector) (values uint32 uint32)) get-block))
(defun get-block (block-num bitvector)
  (with-slots (blocks) bitvector
    (values (aref blocks (+ 0 (* 2 block-num)))
	    (aref blocks (+ 1 (* 2 block-num))))))

(declaim (inline block-rank1))
(defun block-rank1 (pos block-num bitvector)
  (with-slots (block-1bit-count-until-last-select-index) bitvector
    (let ((1bit-count (aref block-1bit-count-until-last-select-index block-num)))
      (if (or (< 1bit-count 32)  ; ブロックにはsel-idxが一つしかない
	      (>= pos 32))       ; ブロック内の最後のsel-idxなのが確実
	  1bit-count
	(logcount (ldb (byte pos 0) (get-block block-num bitvector)))))))

(declaim (ftype (function (positive-fixnum bitvector) (values positive-fixnum array-index)) get-lie-block)
	 (ftype (function (array-index block-number bitvector) (mod 65)) block-rank1))
(declaim (inline get-base-block))
(defun get-base-block (nth bitvector)
  (multiple-value-bind (base-nth base-index)
		       (get-lie-block nth bitvector)
    (multiple-value-bind (block-num offset)
			 (floor base-index +BLOCK-SIZE+)
      (values block-num
	      (- base-nth (block-rank1 offset block-num bitvector))))))

(declaim (inline goto-target-block))
(defun goto-target-block (nth start-block-num precede-1bit-count bitvector)
  (with-slots (block-1bit-count) bitvector
    (loop FOR block-num OF-TYPE fixnum   FROM start-block-num
	  FOR 1cnt      OF-TYPE (mod 65) = (aref block-1bit-count block-num)
	  AND pre-1cnt  OF-TYPE fixnum   = precede-1bit-count THEN (+ pre-1cnt 1cnt)
      WHILE (> nth (+ pre-1cnt 1cnt))
      FINALLY (return (values block-num pre-1cnt)))))

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
	      (t   (+ 32 (the (mod 33) (impl (- nth i) block-high 0 64)))))))))

(declaim (ftype (function (positive-fixnum bitvector) 
			  (values block-number positive-fixnum))
		get-base-block)
	 (ftype (function (positive-fixnum block-number positive-fixnum bitvector) 
			  (values block-number positive-fixnum))
		goto-target-block)
	 (ftype (function (positive-fixnum block-number bitvector)
			  (mod 65))
		block-select1))

(declaim (ftype (function (positive-fixnum bitvector) array-index) select1))
(defun select1 (nth bitvector)
  (declare #.*fastest*
	   (bitvector bitvector)
	   (positive-fixnum nth))
  (multiple-value-bind (base-block-num 1bit-count)
		       (get-base-block nth bitvector)
    (multiple-value-bind (block-num 1bit-count) 
			 (goto-target-block nth base-block-num 1bit-count bitvector)
      (with-slots (block-precede-0bit-count) bitvector
        (the positive-fixnum 
	     (+ 1bit-count                                ; 1bit count
		(the positive-fixnum (aref block-precede-0bit-count block-num)) ; 0bit count
		(block-select1 (- nth 1bit-count) block-num bitvector)))))))

;;;;;;;;;
;;;; rank
(declaim (inline flag-rank1- omitted-block-count block-rank0 rank1))
(defun flag-rank1- (block-num bitvector)
  (with-slots (src-block-all-0bit-flag SBC0F-rank-indices) bitvector
    (multiple-value-bind (idx offset) (floor block-num +WORD-SIZE+)
      (+ (aref SBC0F-rank-indices idx) 
	 (logcount (ldb (byte offset 0) (aref src-block-all-0bit-flag idx)))))))

(defun omitted-block-count (block-num bitvector)
  (the block-number (flag-rank1- block-num bitvector)))

(defun block-rank0 (offset block-num bitvector &aux (end (1+ offset)))
  (multiple-value-bind (block-low block-high) (get-block block-num bitvector)
    (- end
       (if (<= end 32)
	   (logcount (ldb (byte end 0) block-low))
	 (+ (logcount block-low)
	    (logcount (ldb (byte (- end 32) 0) block-high)))))))

(declaim (inline src-all0-block?))
(defun src-all0-block? (block-num bitvector)
  (with-slots (src-block-all-0bit-flag) bitvector
    (multiple-value-bind (idx offset) (floor block-num +WORD-SIZE+)
      (ldb-test (byte 1 offset) (aref src-block-all-0bit-flag idx)))))

(declaim (ftype (function (array-index bitvector) positive-fixnum) rank0 rank1))

(defun rank0 (index bitvector)
  (declare #.*fastest*)
  (multiple-value-bind (src-block-num offset) (floor index +BLOCK-SIZE+)
    (let ((block-num (- src-block-num (omitted-block-count src-block-num bitvector))))
      (let ((pre-0bit-count (aref (block-precede-0bit-count bitvector) block-num)))
	(declare (positive-fixnum pre-0bit-count))
	(print `(,src-block-num ,block-num ,index ,offset ,pre-0bit-count))
	(the positive-fixnum
	     (if (print (src-all0-block? src-block-num bitvector))
		 (+ (- pre-0bit-count
		       (* (1+ (- (- src-block-num block-num) ;(omitted-block-count src-block-num bitvector)
				 (omitted-block-count block-num bitvector)))
			  +BLOCK-SIZE+))
		    (1+ offset))
	       (+ pre-0bit-count
		  (block-rank0 offset block-num bitvector))))))))
      
(defun rank1 (index bitvector)
  (declare #.*fastest*)
  (- (1+ index) (rank0 index bitvector)))

|#
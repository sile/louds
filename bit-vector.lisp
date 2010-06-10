(in-package :louds)

(defparameter *sample-bits*
  (coerce (loop REPEAT 100000 COLLECT (if (zerop (random 3)) 1 0)) 'bit-vector))

(deftype uint32 () '(unsigned-byte 32))
(deftype uint16 () '(unsigned-byte 16))
(deftype uint8  () '(unsigned-byte 8))
(deftype array-index () '(mod #.array-total-size-limit))
(deftype positive-fixnum () '(mod #.most-positive-fixnum))

(defconstant +BLOCK-SIZE+ 64)
(defconstant +WORD-SIZE+  32)
(defconstant +SELECT-INDEX-INTERVAL+ 32)

(deftype block-number () '(mod #.(floor array-total-size-limit +BLOCK-SIZE+)))

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
(defun get-lie-block (nth bitvector)  ;; XXX: 名前
  (with-slots (select-indices) bitvector
    (let ((base-nth (floor nth +SELECT-INDEX-INTERVAL+)))
      (multiple-value-bind (quot rem) (floor base-nth 8)
        (let ((index (+ (ash (aref select-indices (+ 0 (* quot 9)))  0)
			(ash (aref select-indices (+ 1 (* quot 9)))  16))))
	  (unless (zerop rem)
	    (incf index (aref select-indices (+ base-nth quot 1))))

	  (values (* base-nth +SELECT-INDEX-INTERVAL+)
		  index))))))

(declaim (inline get-block))
(declaim (ftype (function (block-number bitvector) (values uint32 uint32)) get-block))
(defun get-block (block-num bitvector)
  (with-slots (blocks) bitvector
    (values (aref blocks (+ 0 (* 2 block-num)))
	    (aref blocks (+ 1 (* 2 block-num))))))

(defun block-rank1 (pos block-num bitvector)
  (with-slots (block-1bit-count-until-last-select-index) bitvector
    (let ((1bit-count (aref block-1bit-count-until-last-select-index block-num)))
      (if (or (< 1bit-count 32)  ; ブロックにはsel-idxが一つしかない
	      (>= pos 32))       ; ブロック内の最後のsel-idxなのが確実
	  1bit-count
	(logcount (ldb (byte pos 0) (get-block block-num bitvector)))))))

(defun get-base-block (nth bitvector)
  (multiple-value-bind (base-nth base-index)
		       (get-lie-block nth bitvector)
    (multiple-value-bind (block-num offset)
			 (floor base-index +BLOCK-SIZE+)
      (values block-num
	      (- base-nth (block-rank1 offset block-num bitvector))))))

(defun goto-target-block (nth start-block-num precede-1bit-count bitvector)
  (with-slots (block-1bit-count) bitvector
    (loop FOR block-num FROM start-block-num
	  FOR 1cnt     = (aref block-1bit-count block-num)
	  AND pre-1cnt = precede-1bit-count THEN (+ pre-1cnt 1cnt)
      WHILE (> nth (+ pre-1cnt 1cnt))
      FINALLY (return (values block-num pre-1cnt)))))

(defun block-select1 (nth block-num bitvector)
  (labels ((impl (nth block beg end)
             (let* ((m (+ beg (floor (- end beg) 2)))
		    (i (logcount (ldb (byte m 0) block))))
	       (cond ((= nth i) (1- (integer-length (ldb (byte m 0) block))))
		     ((< nth i) (impl nth block beg m))
		     (t         (impl nth block m end))))))
    (multiple-value-bind (block-low block-high) (get-block block-num bitvector)
      (let ((i (logcount block-low)))
	(cond ((= nth i) (1- (integer-length block-low)))
	      ((< nth i) (impl nth block-low 0 32))
	      (t   (+ 32 (impl (- nth i) block-high 0 64))))))))

(defun select1 (nth bitvector)
  (multiple-value-bind (base-block-num 1bit-count)
		       (get-base-block nth bitvector)
    (multiple-value-bind (block-num 1bit-count) 
			 (goto-target-block nth base-block-num 1bit-count bitvector)
      (with-slots (block-precede-0bit-count) bitvector
        (+ 1bit-count                                ; 1bit count
	   (aref block-precede-0bit-count block-num) ; 0bit count
	   (block-select1 (- nth 1bit-count) block-num bitvector))))))

;;;;;;;;;
;;;; rank
(declaim (inline flag-rank0 omitted-block-num block-rank0 rank1))
(declaim (ftype (function (block-number bitvector) positive-fixnum) rank0 rank1))
(defun flag-rank0- (block-num bitvector)
  (with-slots (src-block-all-0bit-flag SBC0F-rank-indices) bitvector
    (multiple-value-bind (idx offset) (floor block-num +WORD-SIZE+)
      (+ (aref SBC0F-rank-indices idx) 
	 (logcount (ldb (byte offset 0) (aref src-block-all-0bit-flag idx)))))))

(defun omitted-block-num (index bitvector)
  (the block-number (flag-rank0 (floor index +WORD-SIZE+) bitvector)))

(defun block-rank0 (offset block-num bitvector &aux (end (1+ offset)))
  (multiple-value-bind (block-low block-high) (get-block block-num bitvector)
    (- end
       (if (<= end 32)
	   (logcount (ldb (byte end 0) block-low))
	 (+ (logcount block-low)
	    (logcount (ldb (byte (- end 32) 0) block-high)))))))

(defun rank0 (index bitvector)
  (declare #.*fastest*)
  (multiple-value-bind (block-num offset) (floor index +BLOCK-SIZE+)
    (decf block-num (omitted-block-num index bitvector))
    (let ((pre-0bit-count (aref (block-precede-0bit-count bitvector) block-num)))
      (declare (positive-fixnum pre-0bit-count))
      (the positive-fixnum
	   (+ pre-0bit-count
	      (block-rank0 offset block-num bitvector))))))
      
(defun rank1 (index bitvector)
  (declare #.*fastest*)
  (- (1+ index) (rank0 index bitvector)))
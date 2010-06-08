(in-package :louds)

(defvar *sample-bits*
  (coerce (loop REPEAT 1000 COLLECT (if (oddp (random 5)) 1 0)) 'bit-vector))

(deftype uint32 () '(unsigned-byte 32))
(deftype uint16 () '(unsigned-byte 16))
(deftype uint8  () '(unsigned-byte 8))
(deftype array-index () '(mod #.array-total-size-limit))

(defconstant +BLOCK-SIZE+ 64)
(defconstant +WORD-SIZE+  32)
(defconstant +SELECT-INDEX-INTERVAL+ 32)

(defstruct bitvector
  (blocks                                   t :type (simple-array uint32))
  (block-precede-0bit-count                 t :type (simple-array uint32))
  (block-1bit-count                         t :type (simple-array uint8))
  (block-1bit-count-until-last-select-index t :type (simple-array uint8))
  (select-indices                           t :type (simple-array uint16))
  (src-block-all-0bit-flag                  t :type (simple-array uint32))
  (SBC0F-rank-indices                       t :type (simple-array uint32)))

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

;;;;;;;;;;;
;;;; select
(defun get-base-block (nth bitvector)
  )

(defun goto-target-block (start-block-num precede-1bit-count bitvector)
  (with-slots (block-1bit-count) bitvector
    (loop FOR block-num FROM start-block-num
	  FOR 1cnt     = (aref block-1bit-count block-num)
	  AND pre-1cnt = precede-1bit-count THEN (+ pre-1cnt 1cnt)
      WHILE (> nth (+ pre-1cnt 1cnt))
      FINALLY (return (values block-num pre-1cnt)))))

(defun select1 (nth bitvector)
  (multiple-value-bind (base-block-num 1bit-count)
		       (get-base-block nth bitvector)
    (multiple-value-bind (block-num 1bit-count) 
			 (goto-target-block base-block-num 1bit-count bitvector)
      (+ 1bit-count                                ; 1bit count
	 (aref block-precede-0bit-count block-num) ; 0bit count
	 (block-select1 (- nth 1bit-count) block-num bitvector)))))

#+IGNORE
(defun select1 (nth bv)
  (with-slots (blocks 1bit-counts 0bit-acc-counts) bv
    (multiple-value-bind (div #|rem|#) (floor nth 32)
      (let* ((base-pos (get-base-pos div bv))
	     (base-block-num (* 2 (floor base-pos +BLOCK-SIZE+)))
	     (base-block-low  (aref blocks (+ 0 base-block-num)))
	     (base-block-high (aref blocks (+ 1 base-block-num)))
	     (base-rank1 (get-acc-1bit-count (* div 32)
					     base-pos
					     base-block-low base-block-high
					     bv 
					     (/ base-block-num 2) 
					     )))
	;;(print `(,base-pos ,base-block-num ,base-block-low ,base-block-high ,base-rank1))
	(multiple-value-bind (block-num rank1)
	  (loop FOR block FROM (floor base-block-num 2)
		AND rank1 = base-rank1 THEN (+ rank1 (aref 1bit-counts block))
	    WHILE (> nth (+ rank1 (aref 1bit-counts block)))
	    FINALLY (return (values block rank1)))
	  ;;(print `(,rank1 ,(aref 0bit-acc-counts block-num) ,block-num))
	  (+ rank1
	     (aref 0bit-acc-counts block-num)
	     (select1-block (- nth rank1)
			    (aref blocks (+ 0 (* 2 block-num)))
			    (aref blocks (+ 1 (* 2 block-num))))))))))

#|
;;;;;;;;;;;
;;;; select
(defun get-base-pos (div bv)
  (declare (array-index div)
	   (bv bv)
	   #.*fastest*)
  (with-slots (sel-indices) bv
    (multiple-value-bind (8cnt rem) (floor div 8)
      ;;(print `(,8cnt ,rem ,div))
      (let ((base (+ (aref sel-indices (+ 0 (* 8cnt 8) 8cnt))
		     (aref sel-indices (+ 1 (* 8cnt 8) 8cnt)))))
	(unless (zerop rem)
	  (incf base (aref sel-indices (+ div 8cnt 1))))
	base))))

(defun rank1-block (pos low high)
  (declare ((mod 64) pos)
	   ((unsigned-byte 32) low high)
	   #.*fastest*)
  (the (mod 64)
  (if (< pos 32)
      (logcount (ldb (byte pos 0) low))
    (+ (logcount (ldb (byte 32         0) low))
       (logcount (ldb (byte (- pos 32) 0) high))))))

(defun rank1-block2 (pos low high bv block-num)
  (declare ((mod 64) pos)
	   ((unsigned-byte 32) low high)
	   (bv bv)
	   #.*fastest*)
  (with-slots (1bit-cnt-until-last-selidx) bv
    (let ((last-1bit-cnt (aref 1bit-cnt-until-last-selidx block-num)))
      ;; XXX: 境界条件チェック
      (if (or (< last-1bit-cnt 32)
	      (< pos 32))
	  last-1bit-cnt
	(rank1-block pos low high)))))

(defun get-acc-1bit-count (base-nth base-pos block-low block-high bv block-num)
  (- base-nth (rank1-block2 (mod base-pos +BLOCK-SIZE+)
			    block-low block-high
			    bv
			    block-num
			    )))

;; TODO: tableも使う
;; XXX: 非効率 => 末尾再帰, 8bit以下はlogcountを使わない
(defun select1-block (nth block-low block-high)
  (declare #.*fastest*
	   ((mod 64) nth)
	   ((unsigned-byte 32) block-low block-high))
  (labels ((impl (nth block beg end)
	     (let* ((m (+ beg (floor (- end beg) 2)))
		    (i (logcount (ldb (byte m 0) block))))
	       (declare ((mod 32) m))
	       (cond ((= nth i) (1- (integer-length (ldb (byte m 0) block))))
		     ((< nth i) (impl nth block beg m))
		     ((= m 31)  31) ;; XXX: とりあえずの応急処置
		     (t         (impl nth block m end))))))
    (declare (ftype (function ((mod 33) (unsigned-byte 32) (mod 33) (mod 33)) (mod 33)) impl))
    (let ((i (logcount block-low)))
      (cond ((= nth i) (1- (integer-length block-low)))
	    ((< nth i)
	     (impl nth block-low 0 32))
	    (t (+ 32 (the (mod 33) (impl (- nth i) block-high 0 32))))))))

(defun rank0~ (block-num bv)
  (with-slots (all-0bit-flags a0f-rank-aux) bv
    (multiple-value-bind (div rem) (floor block-num 32)
      (let ((base (aref a0f-rank-aux div)))
	(+ base (logcount (ldb (byte rem 0) (aref all-0bit-flags div))))))))
      
(defun deleted-block-num (pos bv)
  (rank0~ (floor pos 32) bv))

;; NOTE: 0から始まる
(defun select1 (nth bv)
  (with-slots (blocks 1bit-counts 0bit-acc-counts) bv
    (multiple-value-bind (div #|rem|#) (floor nth 32)
      (let* ((base-pos (get-base-pos div bv))
	     (base-block-num (* 2 (floor base-pos +BLOCK-SIZE+)))
	     (base-block-low  (aref blocks (+ 0 base-block-num)))
	     (base-block-high (aref blocks (+ 1 base-block-num)))
	     (base-rank1 (get-acc-1bit-count (* div 32)
					     base-pos
					     base-block-low base-block-high
					     bv 
					     (/ base-block-num 2) 
					     )))
	;;(print `(,base-pos ,base-block-num ,base-block-low ,base-block-high ,base-rank1))
	(multiple-value-bind (block-num rank1)
	  (loop FOR block FROM (floor base-block-num 2)
		AND rank1 = base-rank1 THEN (+ rank1 (aref 1bit-counts block))
	    WHILE (> nth (+ rank1 (aref 1bit-counts block)))
	    FINALLY (return (values block rank1)))
	  ;;(print `(,rank1 ,(aref 0bit-acc-counts block-num) ,block-num))
	  (+ rank1
	     (aref 0bit-acc-counts block-num)
	     (select1-block (- nth rank1)
			    (aref blocks (+ 0 (* 2 block-num)))
			    (aref blocks (+ 1 (* 2 block-num))))))))))

;;;;;;;;;
;;;; rank
(defun rank1 (pos bv)
  (with-slots (0bit-acc-counts blocks) bv
    (let* ((adj-pos (- pos (* (deleted-block-num pos bv) +BLOCK-SIZE+)))
	   (block-num (floor adj-pos +BLOCK-SIZE+))
	   (rem (nth-value 1 (floor adj-pos +BLOCK-SIZE+))))
      (+ (- (* block-num +BLOCK-SIZE+) (aref 0bit-acc-counts block-num))
	 
	 (logcount (ldb (byte (min rem 32) 0)       (aref blocks (+ 0 (* 2 block-num)))))
	 (logcount (ldb (byte (max (- rem 32) 0) 0) (aref blocks (+ 1 (* 2 block-num)))))))))

(defun rank0 (pos bv)
  (- pos (rank1 pos bv)))

#|	      
(defstruct bv
  ;; for select/rank
  (blocks          #() :type (simple-array (unsigned-byte 32)))
  (0bit-acc-counts #() :type (simple-array (unsigned-byte 32)))
  
  ;; for select
  (1bit-counts     #() :type (simple-array (unsigned-byte 8)))
  (sel-indices     #() :type (simple-array (unsigned-byte 16)))

  ;; for rank
  (all-0bit-flags  #() :type (simple-array (unsigned-byte 32)))
  (a0f-rank-aux    #() :type (simple-array (unsigned-byte 32))))  ;; -> rank-indices
|#
(defun total-size (bv)
  (with-slots (blocks 0bit-acc-counts
	       1bit-counts sel-indices
	       all-0bit-flags a0f-rank-aux) bv
    (+ (* (length blocks) 32)
       (* (length 0bit-acc-counts) 32)
       (* (length 1bit-counts) 8)
       (* (length sel-indices) 16)
       (* (length all-0bit-flags) 32)
       (* (length a0f-rank-aux) 32))))
|#
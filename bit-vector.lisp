(in-package :louds)

(defconstant +BLOCK-SIZE+ 64)
(defconstant +SELECT-INDEX-INTERVAL+ 32)

(defvar *sample-bits*
  (coerce (loop REPEAT 1000 COLLECT (if (oddp (random 5)) 1 0)) 'bit-vector))

(deftype index () '(mod #.array-total-size-limit))

(defstruct bv
  ;; for select/rank
  (blocks          #() :type (simple-array (unsigned-byte 32)))
  (0bit-acc-counts #() :type (simple-array (unsigned-byte 32)))
  
  ;; for select
  (1bit-counts     #() :type (simple-array (unsigned-byte 8)))
  (sel-indices     #() :type (simple-array (unsigned-byte 16)))

  (1bit-cnt-until-last-selidx #() :type (simple-array (unsigned-byte 8)))
  
  ;; for rank
  (all-0bit-flags  #() :type (simple-array (unsigned-byte 32)))
  (a0f-rank-aux    #() :type (simple-array (unsigned-byte 32))))  ;; -> rank-indices

(defun bits-to-num (bits start end)
  (loop FOR i FROM start BELOW (min end (length bits))
	FOR b = (bit bits i)
    SUM (ash b (- i start))))

(defun to-u32-blocks (tmp-blocks)
  (coerce
   (loop FOR bs ACROSS tmp-blocks
	 APPEND
	 (loop FOR i FROM 0 BELOW +BLOCK-SIZE+ BY 32
	       COLLECT (bits-to-num bs i (+ i 32))))
   '(vector (unsigned-byte 32))))

(defun count-acc-0bit (tmp-blocks)
  (coerce
   (loop FOR bs ACROSS tmp-blocks
	 FOR 0bit-cnt = (count 0 bs)
	 AND acc = 0 THEN (+ acc 0bit-cnt)
     UNLESS (= +BLOCK-SIZE+ 0bit-cnt)
     COLLECT acc)
   '(vector (unsigned-byte 32))))

(defun count-1bit-per-block (tmp-blocks-rem-all0)
  (coerce
   (loop FOR bs ACROSS tmp-blocks-rem-all0
     COLLECT (count 1 bs))
   '(vector (unsigned-byte 8))))

(defun compress-sel-indices (list &optional acc (base 0) (i 0))
  (if (endp list)
      (coerce (nreverse acc) '(vector (unsigned-byte 16)))
    (destructuring-bind (n . rest) list
      (if (zerop i)
	  (compress-sel-indices rest `(,(ldb (byte 16 0) n) ,(ldb (byte 16 16) n) . ,acc) n 7)
	(compress-sel-indices rest `(,(- n base) . ,acc) base (1- i))))))

(defun calc-sel-indices (tmp-blocks-per-all0)
  (compress-sel-indices
   (cons 0 ;; XXX: きれいじゃない
   (loop WITH nth = 0
	 WITH i   = 0
	 FOR bs ACROSS tmp-blocks-per-all0
     APPEND
     (loop FOR b ACROSS bs
       WHEN (and (incf i)
		 (= b 1)
		 (incf nth)
		 (zerop (mod nth +SELECT-INDEX-INTERVAL+)))
       COLLECT i)))))

(defun calc-all-0bit-flags (tmp-blocks)
  (coerce (loop FOR bs ACROSS tmp-blocks
		COLLECT (if (zerop (count 1 bs)) 1 0))
	  'bit-vector))

(defun to-u32-flags (bits)
  (coerce
   (loop FOR i FROM 0 BELOW (length bits) BY 32
	 COLLECT (bits-to-num bits i (+ i 32)))
   '(vector (unsigned-byte 32))))

(defun calc-aux (flags)
  (coerce
   (loop WITH nth = 0
	 FOR u32 ACROSS flags
     COLLECT (prog1 nth
	       (incf nth (logcount u32))))
   '(vector (unsigned-byte 32))))

(defun calc0 (tmp-blocks-per-all0)
  (coerce 
   ;; TODO: cons 0 ?
   (loop WITH nth = 0
	 WITH i   = 0
	 FOR bs ACROSS tmp-blocks-per-all0
     APPEND
     (loop FOR b ACROSS bs
       WHEN (and (incf i)
		 (= b 1)
		 (incf nth)
		 (zerop (mod nth +SELECT-INDEX-INTERVAL+)))
       COLLECT i))
   '(vector (unsigned-byte 32))))

;; TODO: declaim: (defun rank1-block (pos low high)K(defun select1-block (nth block-low block-high)
(defun calc1 (sel-indices blocks block-size)
  (let ((ary (make-array block-size :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop FOR idx ACROSS sel-indices 
	  FOR block-num = (floor idx +BLOCK-SIZE+)
      DO (setf (aref ary block-num)
	       (rank1-block (mod idx +BLOCK-SIZE+)
			    (aref blocks (+ 0 (* 2 block-num)))
			    (aref blocks (+ 1 (* 2 block-num))))))
    ary))

(defun build-bv (bits &aux (bits-len (length bits)))
  (let* ((tmp-blocks (coerce 
		      (loop FOR i FROM 0 BELOW bits-len BY +BLOCK-SIZE+
			    COLLECT (subseq bits i (min (+ i +BLOCK-SIZE+) bits-len)))
		      'vector))
	 (tmp-blocks-rem-all0 (remove-if (lambda (bs) (every #'zerop bs)) tmp-blocks)))
    (make-bv
     :blocks #2=(to-u32-blocks tmp-blocks-rem-all0)
     :0bit-acc-counts (count-acc-0bit tmp-blocks)
     :1bit-counts (count-1bit-per-block tmp-blocks-rem-all0)
     :sel-indices (calc-sel-indices tmp-blocks-rem-all0)
     :1bit-cnt-until-last-selidx (calc1 (calc0 tmp-blocks-rem-all0) #2# (length tmp-blocks-rem-all0))
     :all-0bit-flags #1=(to-u32-flags (calc-all-0bit-flags tmp-blocks))
     :a0f-rank-aux (calc-aux #1#))))

(defun get-base-pos (div bv)
  (declare (index div)
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

(defun get-acc-1bit-count (base-nth base-pos block-low block-high)
  (- base-nth (rank1-block (nth-value 1 (floor base-pos +BLOCK-SIZE+))
			   block-low block-high)))

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
					     base-block-low base-block-high)))
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
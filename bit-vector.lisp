(in-package :louds)

(defconstant +BLOCK-SIZE+ 64)
(defconstant +SELECT-INDEX-INTERVAL+ 32)

(defvar *sample-bits*
  (coerce (loop REPEAT 1000 COLLECT (if (oddp (random 5)) 1 0)) 'bit-vector))

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
	 FOR acc = 0 THEN (+ acc 0bit-cnt)
     UNLESS (= +BLOCK-SIZE+ 0bit-cnt)
     COLLECT acc)
   '(vector (unsigned-byte 32))))

(defun count-1bit-per-block (tmp-blocks-rem-all0)
  (coerce
   (loop FOR bs ACROSS tmp-blocks-rem-all0
     COLLECT (count 1 bs))
   '(vector (unsigned-byte 8))))

(defun compress-sel-indices (list &optional acc (base 0) (i 7))
  (if (endp list)
      (coerce (nreverse acc) '(vector (unsigned-byte 16)))
    (destructuring-bind (n . rest) list
      (if (zerop i)
	  (compress-sel-indices rest `(,(ldb (byte 16 0) n) ,(ldb (byte 16 16) n) . ,acc) n 7)
	(compress-sel-indices rest `(,(- n base) . ,acc) base (1- i))))))

(defun calc-sel-indices (tmp-blocks-per-all0)
  (compress-sel-indices
   (loop WITH nth = 0
	 WITH i   = 0
	 FOR bs ACROSS tmp-blocks-per-all0
     APPEND
     (loop FOR b ACROSS bs
       WHEN (and (incf i)
		 (= b 1)
		 (incf nth)
		 (zerop (mod nth +SELECT-INDEX-INTERVAL+)))
       COLLECT i))))

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

(defun build-bv (bits &aux (bits-len (length bits)))
  (let* ((tmp-blocks (coerce 
		      (loop FOR i FROM 0 BELOW bits-len BY +BLOCK-SIZE+
			    COLLECT (subseq bits i (min (+ i +BLOCK-SIZE+) bits-len)))
		      'vector))
	 (tmp-blocks-rem-all0 (remove-if (lambda (bs) (every #'zerop bs)) tmp-blocks)))
    (make-bv
     :blocks (to-u32-blocks tmp-blocks-rem-all0)
     :0bit-acc-counts (count-acc-0bit tmp-blocks)
     :1bit-counts (count-1bit-per-block tmp-blocks-rem-all0)
     :sel-indices (calc-sel-indices tmp-blocks-rem-all0)
     :all-0bit-flags #1=(to-u32-flags (calc-all-0bit-flags tmp-blocks))
     :a0f-rank-aux (calc-aux #1#))))

(defun log2 (x)
  (log x 2))

(defun collect-1bit-index (bits)
  (loop FOR i FROM 0
	FOR b ACROSS bits
	WHEN (= b 1)
	COLLECT i))

(defun max-difference (1bit-indices)
  (loop FOR i FROM 1 BELOW (length 1bit-indices)
	MAXIMIZE (- (aref 1bit-indices (- i 0))
		    (aref 1bit-indices (- i 1)))))

(defun calc-c (1bit-indices bits-length)
  (loop WITH dif = (max-difference 1bit-indices)
	FOR C FROM 1
    WHILE (> dif (expt (log2 bits-length) C))
    FINALLY (return C)))
	
(defun ready-select1 (bits)
  (let* ((1bit-indices (coerce (collect-1bit-index bits) 'vector))
	 (M (length bits))
	 (c (calc-c 1bit-indices (length bits)))
	 (interval (round (/ (log2 M)
			     (* 2 C (log2 (log2 M)))))))
    (values m c interval (length 1bit-indices))))
    
(defparameter *bv* 
  (coerce (loop FOR i FROM 0 BELOW 10000000
		COLLECT (if (zerop (random 3)) 1 0)) ;(mod i 2))
	  'bit-vector))

(defparameter M (length *bv*))
(defparameter S (coerce (loop FOR i FROM 0
			      FOR b ACROSS *bv*
			      WHEN (= b 1)
			      COLLECT i)
			'vector))

(defparameter C 1)
(defparameter *T* (round (/ (log2 M) 
			    (* 2 C (log2 (log2 M))))))
(defparameter Z (ceiling #|round|# (* C (log2 (log2 M)))))
(defparameter ST (loop FOR i FROM 0 BELOW (length S) BY *t*
		       COLLECT (aref S i)))

(defparameter A1 (coerce ST 'vector))
(defparameter A2 (coerce (cons 0 (loop FOR (a b) ON (coerce S 'list)
				       WHILE b
				       COLLECT (- b a)))
			 'vector))
(defun select (i)
  (let ((i~  (floor i *t*))
	(i~~ (mod i *t*)))
    (let ((x (aref A1 i~))
	  (y (loop FOR j FROM (1+ (- i i~~)) TO i SUM (aref A2 j))))
      (print `(,i~ ,i~~ ,x ,y))
      (+ x y))))

(defun sel (i aa1 aa2)
  (declare (optimize (speed 3) (safety 0))
	   (unmuffle-conditions compiler-note)
	   ((mod #.array-total-size-limit) i)
	   ((simple-array (unsigned-byte 16)) aa1)
	   ((simple-array (unsigned-byte 8)) aa2))
  (multiple-value-bind (i~ i~~) (floor i 4)
    (+ (aref aa1 i~)
       (loop WITH x OF-TYPE (mod #.(ash array-total-size-limit -1)) = 0
	     FOR j FROM (1+ (- i i~~)) TO i
	     DO (incf x (aref aa2 j))
	     FINALLY (return x)))))


;;; select
;; 1] i番目の1ビットの位置を予め計算しておく
;; 2] その際に以下の二点が云々
;; 2-1] 位置情報(整数)は、それを保持するのに最低限必要なビット数で表現する(4byteは使わない)
;; 2-2] 数が多い場合は、位置情報を直接保持するのではなく、T間隔で間引く
;; 2-3] 間引いた分は、連接位置間の差分情報で補完する


(defun gen-a3-blocks (n)
  (loop FOR i FROM *t* BELOW n BY *t*
    COLLECT (loop FOR j FROM (1+ (- i *t*)) TO i COLLECT j)))

(defparameter A3
  (coerce (mapcar (lm (length (intersection $ st))) (gen-a3-blocks M))
	  'vector))
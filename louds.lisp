(defpackage louds
  (:use :common-lisp))
(in-package :louds)

	   
(defun select0 (bits i)
  "BITSの中のi番目の0の位置を返す。iが0以下、もしくはi番目の0がBITSに存在しない場合は-1を返す。"
  (loop FOR pos FROM 0
	FOR bit ACROSS bits
    WHEN (= 0 bit)
    DO (when (zerop (decf i))
	 (return pos))
    FINALLY (return -1)))

; ノードNのビット配列内での位置
(defun select1 (bits i)
  "BITSの中のi番目の1の位置を返す。iが0以下、もしくはi番目の1がBITSに存在しない場合は-1を返す。"
  (loop FOR pos FROM 0
	FOR bit ACROSS bits
    WHEN (= 1 bit)
    DO (when (zerop (decf i))
	 (return pos))
    FINALLY (return -1)))
	
; 位置Xにあるビットがどのノードに対応するか
(defun rank0 (bits x)
  "BITS内で、位置Xよりも左(Xも含む)にある0ビットの数を返す。"
  (loop FOR i FROM x DOWNTO 0
	FOR bit = (aref bits i)
    WHEN (= 0 bit)
    SUM 1))

(defun rank1 (bits x)
  "BITS内で、位置Xよりも左(Xも含む)にある1ビットの数を返す。"
  (loop FOR i FROM x DOWNTO 0
	FOR bit = (aref bits i)
    WHEN (= 1 bit)
    SUM 1))

;; lbs = louds bit string
(use-package :common-utils)

(defvar *tree* 
  '(:a (:b)
       (:c (:f) 
           (:g (:i))) 
       (:d) 
       (:e (:h))))
;; (:a (:b) (:c (:f) (:g (:i))) (:d) (:e (:h)))
(defun tree-to-lbs (tree)
  (coerce
   (loop WITH  tree-que = `((:super-root ,tree))
         WHILE tree-que
     APPEND
     (destructuring-bind ((node-name . children) . rest-trees) tree-que
       (declare (ignore node-name))
       (setf tree-que (append rest-trees children))
       (append (loop REPEAT (length children) COLLECT 1) 
               '(0))))
   'bit-vector))
                 
(defun run (bits start bit)
  (let* ((beg (position bit            bits :start start))
	 (end (or (position (logxor 1 bit) bits :start beg)
		  (length bits))))
    (list (- end beg)
	  (position bit bits :start end))))

(defun partition (lbs)
  (let ((r0 (loop FOR (length next) = (run lbs (or next 0) 0)
		  APPEND `(,@(loop REPEAT (1- length) COLLECT 0) 1)
		  WHILE next))
	(r1 (loop FOR (length next) = (run lbs (or next 0) 1)
		  APPEND `(,@(loop REPEAT (1- length) COLLECT 0) 1)
		  WHILE next)))
    (values (coerce r0 'bit-vector)
	    (coerce r1 'bit-vector))))

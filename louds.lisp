(defpackage louds
  (:use :common-lisp))
(in-package :louds)

(defun tree-to-lbs (tree &aux names)
  (values
   (coerce
    (loop WITH  tree-que = `((:super-root ,tree))
          WHILE tree-que
      APPEND
      (destructuring-bind ((node-name . children) . rest-trees) tree-que
        (push node-name names)
        (setf tree-que (append rest-trees children))
        (append (loop REPEAT (length children) COLLECT 1) 
                '(0))))
   'bit-vector)
   (coerce (reverse names) 'vector)))
                 
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

(defstruct (pnode (:conc-name ""))
  r0
  r1
  node-num
  parent-node)

(defun tree-to-pnode (tree)
  (multiple-value-bind (r0 r1) (partition (tree-to-lbs tree))
    (make-pnode :r0 r0
		:r1 r1
		:node-num 1
		:parent-node 1)))

(defun select1 (bits i)
  "i番目の1の位置を返す"
  (loop FOR pos FROM 0
	FOR bit ACROSS bits
    WHEN (= 1 bit)
    DO (when (zerop (decf i))
	 (return pos))
    FINALLY (return -1)))

(defun rank1- (bits last)
  (loop FOR i FROM 0 BELOW last
	FOR bit ACROSS bits
    WHEN (= 1 bit)
    SUM 1))

(defvar *tree* 
  '(:a (:b)
       (:c (:f) 
           (:g (:i))) 
       (:d) 
       (:e (:h))))

(defun isleaf (node)
  (with-slots (r0 node-num) node
    (= 0 (bit r0 (1- node-num)))))

(defun next-sibling (node)
  (with-slots (r1 node-num parent-node) node
    (when (= 0 (bit r1 (1- node-num)))
      (incf node-num)
      (unless (isleaf node)
	(incf parent-node))
      node)))

(defun prev-sibling (node)
  (with-slots (r1 node-num parent-node) node
    (when (= 0 (bit r1 (- node-num 2)))
      (unless (isleaf node)
	(decf parent-node))
      (decf node-num)
      node)))

(defun first-child (node)
  (unless (isleaf node)
    (with-slots (r0 r1 node-num) node
      (let ((parent (rank1- r0 node-num)))        
        (setf node-num (+ 2 (select1 r1 parent))))
      node)))

(defun last-child (node)
  (unless (isleaf node)
    (with-slots (r0 r1 node-num) node
      (let ((parent (rank1- r0 node-num)))
        (setf node-num (1+ (select1 r1 (1+ parent)))))
      node)))

(defun parent(node)
  (with-slots (r0 r1 node-num parent-node) node
    (let ((parent (rank1- r1 (1- node-num)))) 
      (setf node-num (1+ (select1 r0 parent))))
    node))

#|
select1(LBS, i) 
: ノードiに対応する1bitが、LBS内に占める位置

=>

select1(R0,rank1-(R1,i))+i
: 
 rank1-(R1, i): ノードiよりも前にある(=上位階層の?)親ノードの数を求める = j
 select1(R0,j): jに対応する(親)ノードを求める = k
 k+i: LBSでの位置, i=LBS内にある1bitの数=当然i個, k=LBS内にある0bitの数=親ノードの数

つまり、
select1(LBS, i)は、
ノード番号+左にある親ノードの数 = LBS内での位置、を表しており、
select1(R0,rank1-(R1,i))は、親ノード番号(左にある親ノードの数)を取得可能なので云々
|#
#|
select0(LBS, i)
: ノードiに対応する0bitが、LBS内に占める位置

=>
select1(R1, rank1-(R0,i))+i
|#
#|
select1(R0,rank1-(R1,i)) => ノードiに対応する0bitの数 => 対応って?
select1(R1,rank1-(R0,i)) => ノードiに対応する1bitの数
|#

#|
;; double-numbering なしバージョン
(defun isleaf (node)
  (with-slots (r0 node-num) node
    (= 0 (bit r0 (1- node-num)))))

(defun next-sibling (node)
  (with-slots (r1 node-num parent-node) node
    (when (= 0 (bit r1 (1- node-num)))
      (incf node-num)
      node)))

(defun prev-sibling (node)
  (with-slots (r1 node-num parent-node) node
    (when (= 0 (bit r1 (- node-num 2)))
      (decf node-num)
      node)))

(defun first-child (node)
  (unless (isleaf node)
    (with-slots (r0 r1 node-num) node
      (let ((parent (rank1- r0 node-num))) ; 自分が何番目の親かを取得する => n
        (setf node-num (+ 2 (select1 r1 parent)))) ; n番目の親の子ノードを取得する
      node)))

(defun last-child (node)
  (unless (isleaf node)
    (with-slots (r0 r1 node-num) node
      (let ((parent (rank1- r0 node-num)))
        (setf node-num (1+ (select1 r1 (1+ parent)))))
      node)))

(defun parent(node)
  (with-slots (r0 r1 node-num parent-node) node
    (setf node-num (1+ (select1 r0 (rank1- r1 (- node-num 1))))) ; 自分の親が何番目の親かを取得する => n
    node))
|#

#|
r0でi番目の1は、1番目の親ノードに対応する
|#

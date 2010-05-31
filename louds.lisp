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
           
(defun run-length (lbs start bit &aux (~bit (logxor 1 bit)))
  (let* ((beg (position  bit lbs :start start))
         (end (position ~bit lbs :start beg)))
    (setf end (or end (length lbs)))
    (list (- end beg)
          (position bit lbs :start end))))

(defun partition (lbs)
  (flet ((rX (bit)
           (loop FOR (length next) = (run-length lbs (or next 0) bit)
                 FOR 0bits = (loop REPEAT (1- length) COLLECT 0)
                 APPEND `(,@0bits 1)
                 WHILE next)))
    (values (coerce (rX 0) 'bit-vector)    ; r0
            (coerce (rX 1) 'bit-vector)))) ; r1

(defstruct louds++ 
  r0     ; r0
  r1     ; r1
  names) ; 各ノードの名前を保持する配列

(defun tree-to-louds++ (tree)
  (multiple-value-bind (lbs names) (tree-to-lbs tree)
    (multiple-value-bind (r0 r1) (partition lbs)
      (make-louds++ :r0 r0
                    :r1 r1
                    :names (subseq names 1)))))

(defun node-name (node-num louds++)
  (with-slots (names) louds++
    (aref names node-num)))

(defun isleaf (node-num louds++)
  (with-slots (r0) louds++
    (= 0 (bit r0 node-num))))

(defun next-sibling (node-num louds++)
  (with-slots (r1) louds++
    (when (= 0 (bit r1 node-num))
      (1+ node-num))))

(defun prev-sibling (node-num louds++)
  (with-slots (r1) louds++
    (when (= 0 (bit r1 (1- node-num)))
      (1- node-num))))

(defun first-child (node-num louds++)
  (unless (isleaf node-num louds++)
    (with-slots (r0 r1) louds++
      (let ((nth-parent (rank1 r0 node-num))) ; node-numが何個目の親かを取得する
        (1+ (select1 r1 nth-parent))))))

(defun last-child (node-num louds++)
  (unless (isleaf node-num louds++)
    (with-slots (r0 r1) louds++
      (let ((nth-parent (rank1 r0 node-num))) ; node-numが何個目の親かを取得する
        (select1 r1 (1+ nth-parent))))))

(defun parent(node-num louds++)
  (with-slots (r0 r1) louds++
    (let ((nth-parent (rank1- r1 node-num)))  ; node-numの親が、何個目の親かを取得する
      (select1 r0 nth-parent))))

(defstruct (pnode (:conc-name ""))
  r0
  r1
  node-num)

(defun tree-to-pnode (tree)
  (multiple-value-bind (r0 r1) (partition (tree-to-lbs tree))
    (make-pnode :r0 r0
		:r1 r1
		:node-num 0)))


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

(defun rank1 (bits last)
  (loop FOR i FROM 0 TO last
	FOR bit ACROSS bits
    WHEN (= 1 bit)
    SUM 1))

(defvar *tree* 
  '(:a (:b)
       (:c (:f) 
           (:g (:i))) 
       (:d) 
       (:e (:h))))


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

(defun louds++-traverse (louds++ &optional (root-node 0) (level 0))
  (format t "~&~V@T~S~%" level (node-name root-node louds++))
  (unless (isleaf root-node louds++)
    (loop FOR child-node = (first-child root-node louds++)
                      THEN (next-sibling child-node louds++)
      WHILE child-node
      DO
      (louds++-traverse louds++ child-node (1+ level)))))

(defpackage louds
  (:use :common-lisp))
(in-package :louds)

	   
(defun select0 (lbs i)
  "LBSの中のi番目の0の位置を返す。iが0以下、もしくはi番目の0がLBSに存在しない場合は-1を返す。"
  (loop FOR pos FROM 0
        FOR bit ACROSS lbs
    WHEN (= 0 bit)
    DO (when (zerop (decf i))
         (return pos))
    FINALLY (return -1)))

; ノードNのビット配列内での位置
(defun select1 (lbs i)
  "LBSの中のi番目の1の位置を返す。iが0以下、もしくはi番目の1がLBSに存在しない場合は-1を返す。"
  (loop FOR pos FROM 0
        FOR bit ACROSS lbs
    WHEN (= 1 bit)
    DO (when (zerop (decf i))
         (return pos))
    FINALLY (return -1)))
        
; 位置Xにあるビットがどのノードに対応するか
(defun rank0 (lbs x)
  "LBS内で、位置Xよりも左(Xも含む)にある0ビットの数を返す。"
  (loop FOR i FROM x DOWNTO 0
        FOR bit = (aref lbs i)
    WHEN (= 0 bit)
    SUM 1))

(defun rank1 (lbs x)
  "LBS内で、位置Xよりも左(Xも含む)にある1ビットの数を返す。"
  (loop FOR i FROM x DOWNTO 0
        FOR bit = (aref lbs i)
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

(defvar *lbs* (tree-to-lbs *tree*))
(defvar *r0*)
(defvar *r1*)
(setf (values *r0* *r1*) (partition *lbs*))


;; 0が連接するということは、(1がないので)子ノードを持たないということ
;; そのためr0で0が出ているのノードは、葉ノード
(defun is-leaf (r0 x)
  (= (aref r0 x) 0))

;; => r0は親子関係をエンコード?
;; => r1は兄弟関係をエンコード?

(defun next-sibling (r1 x)
  (unless (= 1 (aref r1 x))
    (1+ x)))
	     
(defun prev-sibling (r1 x)
  (unless (or (zerop x)
	      (= 1 (aref r1 (1- x))))
    (1- x)))
  

(defun parent (x))
(defun first-child(x))
(defun last-child (x))

(:A (:B) 
    (:C (:F) 
	(:G (:I))) 
    (:D) 
    (:E (:H)))

(defun select1 (bits i)
  "i番目の1の位置を返す"
  (loop FOR pos FROM 0
	FOR bit ACROSS bits
    WHEN (= 1 bit)
    DO (when (zerop (decf i))
	 (return pos))
    FINALLY (return -1)))
	
(defun rank1- (bits x)
  "BITS内で、位置Xよりも左(Xも含む)にある1ビットの数を返す。"
  (loop FOR i FROM (- x 2) DOWNTO 0
	FOR bit = (aref bits i)
    WHEN (= 1 bit)
    SUM 1))

;; (select1 lbs i) = (+ (select1 r0 (rank1- r1 i)) i)

;; (select1 r0 (rank1- r1 i)) => iノードの親を取得することに等しい
;; ↑とiを足すのは、lsbの親の番号(0でエンコード)と子の番号(1でエンコード)を足すのに等しい
;;  i = 子の番号 = 1の数 => 1はi個あるのが当たり前
;; ===> つまり、ノード番号から、lbs内での位置が取得できる

;; rank = (id-of position)
;; select = (position-of id)

;; If pos = (position0-of id) then (id0-of pos) = id and (id1-of pos) = (- pos id0)
;; (0-position-of id0) = pos0
;; (0-id-of pos0) = id0
;; (1-id-of pos0) = (- pos0 id0) = id1

;; [まとめ]
;; LOUDS++は、大体以下の三つからなる
;; 1) ノード番号=>LBS内での位置、という対応を保持 (DoubleNumbering one-or-zero based)
;; 2) select = LBS内での位置 => ノード番号取得
;; 3) Patritioned => 親子関係、兄弟関係をr0、r1に分離
;; +++) bit-vector

;; [その他]
;; (rank0 position1) = (parent-node-of position1) => position1に対応するノードの親ノード番号を取得
;; (rank0 position0) = (node-of position0) => position0のノード番号を取得する
;; (rank1 position1) = (node-of position1) => position1のノード番号を取得する
;; (select0 id) => ノード番号IDのLBS中での0表現の位置を取得する
;; (select1 id) => ノード番号IDのLBS中での1表現の位置を取得する
;; position1 = (- position0 id)

(defun isleaf(lbs node-num)
  (let* ((node-bit (select0 lbs node-num))         ; 対応する0ビットを取得
         (child-node-bit (bit lbs (1+ node-bit)))) ; 最初の子ノード(候補)を取得する
    (= 0 child-node-bit)))  ; 子ノードは、node-bitに続く1ビットで表現されているので、それが0の場合は、子ノードなしと判断できる

(defun parent(lbs node-num)
  (let ((node-bit (select1 lbs node-num)))  ; 対応する0ビットを取得
    (rank0 lbs node-bit)))                     ; 親のノード番号を取得

(defun first-child(lbs node-num)
  (unless (isleaf lbs node-num)
    (let ((parent-bit (select0 lbs node-num))) ; 対応する0ビットを取得
      (rank1 lbs (1+ parent-bit)))))           ; 最初の子ノードを取得する

(defun last-child(lbs node-num)
  (unless (isleaf lbs node-num)
    (let ((next-parent-bit (select0 lbs (1+ node-num)))) ; 一つ後の0ビットを取得する
      (rank1 lbs (1- next-parent-bit)))))                ; 最後の子ノードを取得する

(defun next-sibling (lbs node-num)
  (let* ((node-bit (select1 lbs node-num))   ; 対応する1ビットを取得
         (next-bit (bit lbs (1+ node-bit)))) ; 一つ右のノードを取得する
    (when (= 1 next-bit)  ; 1ビットが続く場合は兄弟あり
      (1+ node-num))))

(defun prev-sibling (lbs node-num)
  (let* ((node-bit (select1 lbs node-num))   ; 対応する1ビットを取得
         (prev-bit (bit lbs (1- node-bit)))) ; 一つ左のノードを取得する
    (when (= 1 prev-bit)  ; 1ビットが続く場合は兄弟あり
      (1- node-num))))


(defun lbs-traverse (lbs node-names &optional (parent-node 1) (level 0))
  (format t "~&~V@T~S~%" level (aref node-names parent-node))
  (unless (isleaf lbs parent-node)
    (loop FOR child-node = (first-child lbs parent-node) 
                     THEN (next-sibling lbs child-node)
      WHILE child-node
      DO 
      (lbs-traverse lbs node-names child-node (1+ level)))))
			 

(defstruct node
  node-number
  position-in-lbs)
(defun log2 (x)
  (log x 2))

(defparameter *bv* 
  (coerce (loop FOR i FROM 0 BELOW 70000 COLLECT (mod i 2))
	  'bit-vector))

(defparameter M (length *bv*))
(defparameter S (loop FOR i FROM 0
		      FOR b ACROSS *bv*
		      WHEN (= b 1)
		      COLLECT i))

(defparameter C 1)
(defparameter *T* (round (/ (log2 M) 
			    (* 2 C (log2 (log2 M))))))
(defparameter Z (ceiling #|round|# (* C (log2 (log2 M)))))
(defparameter ST (loop FOR i FROM 0 BELOW (length S) BY *t*
		       COLLECT (elt S i)))

(defparameter A1 (coerce ST 'vector))
(defparameter A2 (coerce (loop FOR (a b) ON S 
			       WHILE b
			       COLLECT (- b a))
			 'vector))
(defparameter T1 
  (coerce
   (loop FOR i FROM 0 BELOW (ash 1 (* *T* Z))
     COLLECT
     (loop FOR j FROM 0 BELOW *T*
	   SUM (ldb (byte Z (* Z j)) i)))
   'vector))

(defun select (i)
  (let ((i~  (floor i *t*))
	(i~~ (mod i *t*)))  ; orig: (mod i *t*)
    (let ((x (if (> i~ 0) (aref A1 i~) 0))
	  (y (loop FOR j FROM 1 TO i~~ SUM (aref A2 (+ i~ j)))))
      (print `(,i~ ,i~~ ,x ,y))
      (+ x (aref T1 y)))))


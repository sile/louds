(defparameter *bv* #*10101001010010100011101101101010101110101010110010101)
(defparameter M (length *bv*))
(defparameter S (loop FOR i FROM 0
		      FOR b ACROSS *bv*
		      WHEN (= b 1)
		      COLLECT i))

(defparameter C 1)
(defparameter *T* (floor (/ (log M 2) 
			    (* 2 C (log (log M 2) 2)))))
(defparameter Z (floor (* C (log (log M 2) 2))))
(defparameter ST (loop FOR i FROM *t* BELOW (length S) BY *t*
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
  (let ((i~  (/ i *t*))
	(i~~ (mod (1+ i) *t*)))
    (let ((x (if (> i~ 0) (aref A1 (floor i~)) 0))
	  (y (loop FOR j FROM 1 TO i~~ SUM (aref A2 (+ (floor i~) j)))))
      
      (values i~ i~~ x y))))


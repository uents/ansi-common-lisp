;;;;
;;;; @brief 4. 特別なデータ構造
;;;; @see ANSI Common Lisp
;;;;

;;;;------------------------------------
;;;; 4.1 配列
;;;;------------------------------------

;;; 初期値なしで配列を生成
(setf arr (make-array '(2 3)
					  :initial-element nil))
; -> #2A((NIL NIL NIL) (NIL NIL NIL))

;;; 0行0列目にアクセス
(aref arr 0 0)

;;; 要素を書き換え
(setf (aref arr 0 0) 'b)

;;; 1次元配列はベクターと呼ばれる
(setf vec (make-array 4 
					  :initial-element nil))
; -> #(NIL NIL NIL NIL)

;;; ベクターへのアクセス
(svref vec 0)


;;;;------------------------------------
;;;; 4.2 例：二分探索
;;;;------------------------------------

(defun bin-search (target vec)
  (let ((len (length vec)))
	(and (not (zerop len))
		 (finder target vec 0 (- len 1)))))

(defun finder (target vec start end)
  (format t "~A ~%" (subseq vec start (+ end 1)))
  (let ((range (- end start)))
	(if (zerop range)
		(if (eql target (aref vec start))
			target
		  nil)
	    (let ((mid (+ start (round (/ range 2)))))
		  (let ((target2 (aref vec mid)))
			(if (< target target2)
				(finder target vec start (- mid 1))
			    (if (> target target2)
					(finder target vec (+ mid 1) end)
				    target)))))))


;;;;------------------------------------
;;;; 4.3 ストリングと文字
;;;;------------------------------------




;;;;------------------------------------
;;;; 4.x 練習問題
;;;;------------------------------------

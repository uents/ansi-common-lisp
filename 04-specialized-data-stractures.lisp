;;;;
;;;; @brief 4. 特別なデータ構造
;;;; @see ANSI Common Lisp
;;;;

;;;;------------------------------------
;;;; 4.1 配列
;;;;------------------------------------

;;; 初期値なしで配列を生成
(setf arr (make-array '(2 3) :initial-element nil))
; -> #2A((NIL NIL NIL) (NIL NIL NIL))

;;; 0行0列目にアクセス
(aref arr 0 0)

;;; 要素を書き換え
(setf (aref arr 0 0) 'b)

;;; 1次元配列はベクターと呼ばれる
(setf vec (make-array 4 :initial-element nil))
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

(sort "elbow" #'char<) ;; -> "below"
(aref "abc" 1) ;; -> #\b
(char "abc" 1) ;; -> #\b

;;; copy-seq はその名の通りシーケンスをコピーして返す
(let ((str (copy-seq "Merlin")))
  (setf (char str 3) #\k)
  str)
;; -> "Merkin"

;;; string-equal は大文字/小文字を区別しない
(equal "fred" "Fred")  ;; -> NIL
(string-equal "fred" "Fred")  ;; -> T


;;;;------------------------------------
;;;; 4.4 シーケンス
;;;;------------------------------------

;;; elt : リスト/ベクター/ストリングのいずれの種類のシーケンスにも適用できる
(setf lst '(a b c d e))
(setf vec (make-array 5 :initial-contents '(a b c d e)))
(setf str "abcde")

(elt lst 1) ;; -> B
(elt vec 1) ;; -> B
(elt str 1) ;; -> #\b

(defun mirror? (s)
  (let ((len (length s)))
	(and (evenp len)
		 (do ((forward 0 (+ forward 1))
			  (back (- len 1) (- back 1)))
			 ((or (> forward back)
				  (not (eql (elt s forward)
							(elt s back))))
			  (> forward back))))))

(mirror? '(a b b a)) ;; -> t
(mirror? (make-array 4 :initial-contents '(a b b a))) ;; -> t
(mirror? "abba") ;; -> t

;;; position : シーケンスの中で要素の位置を返す
(position #\a "fantasia") ;; -> 1
(position #\a "fantasia" :start 3 :end 5) ;; -> 4
(position #\a "fantasia" :from-end t) ;; -> 7

(position 'c '((c d) (a b)) :key #'car) ;; -> 0
(position 'a '((c d) (a b)) :key #'car) ;; -> 1
(position 'd '((c d) (a b)) :key #'car) ;; -> nil
(position 'b '((c d) (a b)) :key #'car) ;; -> nil

(position '(a b) '((c d) (a b)) :test #'equal) ;; -> 1

(position 3 '(1 0 7 5) :test #'<) ;; -> 2


;; 最初の空白と次の空白までの間の文字列を抜き出す
(defun second-word (str)
  (let ((p1 (+ (position #\  str) 1)))
	(subseq str p1 (position #\  str :start p1))))

;; position-if は第1引数の述語を満たす要素を見つける
;;  この関数は :test 以外の全てのキーワード引数を取る
(position-if #'oddp '(2 3 4 5)) ;; -> 1

;; find, find-if : member, member-if のシーケンス版
(find #\a "cat") ;; -> #\a
(find 'a (make-array 5 :initial-contents '(a b c d e))) ;; -> 'a
(find 'a '(a b c d)) ;; -> 'a

(find-if #'characterp "ham") ;; -> #\h


;; remove, remove-if : シーケンスにも適用可能

;; remove-duplicates : シーケンスから要素の重複を除いて、
;;                     各要素について最後の出現したものを返す
(remove-duplicates "abracadabra") ;; -> "cdbra"


;; reduce : シーケンスの値をひとつにまとめる
(reduce #'+ '(1 2 3 4 5 6)) ;; -> 21

; lengthもこう書ける
(defun my-length (lst)
  (reduce #'(lambda (accum elem)
			  (progn
				(format t "acc:~A elem:~A ~%" accum elem)
				(+ accum 1)
				))
		  lst
		  :initial-value 0))


;;;;------------------------------------
;;;; 4.5 例：日付解析
;;;;------------------------------------

;; alpha-char-p : 文字がアルファベットの場合にtrueを返す
(alpha-char-p #\a) ;; -> t


;; (tokens "ab12 3cde.f" #'alpha-char-p 0)
;;  -> ("ab" "cde" "f")

(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
	(if p1
		(let ((p2 (position-if #'(lambda (c)
								   (not (funcall test c)))
							   str :start p1)))
		  (cons (subseq str p1 p2)
				(if p2
					(tokens str test p2)
				    nil)))
	    nil)))

(defun consistuent (c)
  (and (graphic-char-p c)
	   (not (char= c #\  ))))


;; (parse-date "16 Aug 1980")
;;  -> (16 8 1980)

(defun parse-date (str)
  (let ((toks (tokens str #'consistuent 0)))
	(list (parse-integer (first toks))
		  (parse-month (second toks))
		  (parse-integer (third toks)))))

(defun consistuent (c)
  (and (graphic-char-p c)
	   (not (char= c #\  ))))

(defconstant month-names
  #("jan" "feb" "mar" "apr" "may" "jun"
	"jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str)
  (let ((p (position str month-names
					 :test #'string-equal)))
	(if p
		(+ p 1)
	    nil)))



;;;;------------------------------------
;;;; 4.6 ストラクチャ
;;;;------------------------------------

;;; defstruct で point を定義すると
;;; make-point, copy-point, point-x, point-y, point-p のような
;;; 関数も同時に定義される
(defstruct point
  x
  y)

(setf p (make-point :x 0 :y 0))
;; -> #S(POINT :X 0 :Y 0)

(point-p p) ;; -> T
(typep p 'point) ;; -> T


(defstruct polemic
  (type (progn
		  (format t "what kind of polemic was it? ")
		  (read)))
  (effect nil))

(setf p (make-polemic))
;; what kind of polemic was it? foo
;; -> #S(POLEMIC :TYPE FOO :EFFECT NIL)
;;
;; typeは標準入力値、effectはデフォルト値が指定される

(polemic-type p)   ;; -> FOO
(polemic-effect p) ;; -> NIL


(defstruct (point (:conc-name p)
				  (:print-function print-point))
  (x 0)
  (y 0))

(defun print-point (p stream depth)
  (format stream "#<~A,~A>" (px p) (py p)))

(setf p (make-point)) ;; -> #<0,0>
(px p) ;; -> 0
(px y) ;; -> 0

(setf q (make-point :x 1 :y 2)) ;; -> #<1,2>
(setf r (make-point :x 3 :y 4 :z 5)) ;; -> エラー

;; conc-name でフィールドのアクセス関数名を変えているため
;; (point-x p) (point-y p) などはエラーする



;;;;------------------------------------
;;;; 4.7 例：二分探索木
;;;;------------------------------------

(defstruct (node (:print-function
				  (lambda (n stream depth)
					(format stream "#<~A>" (node-elt n)))))
  elt
  (l nil)
  (r nil))

(defun bst-insert (obj bst test)
  (if (null bst)
	  (make-node :elt obj)
	  (let ((elt (node-elt bst)))
		(if (eql obj elt)
			bst
		    (if (funcall test obj elt)
				(make-node
				 :elt elt
				 :l (bst-insert obj (node-l bst) test)
				 :r (node-r bst))
			    (make-node
				 :elt elt
				 :l (node-l bst)
				 :r (bst-insert obj (node-r bst) test))
				)))))



(defun bst-print (bst &optional (depth 0))
  (if (null bst)
	  (format t (concatenate 'string "~" (format nil "~A" (* depth 2)) "~#nil~%"))
	  (progn
		(format t (concatenate 'string "~" (format nil "~A" (* depth 2)) "~#<~A>~%")
				(node-elt bst))
		(bst-print (node-l bst) (+ depth 1))
		(bst-print (node-r bst) (+ depth 1)))
	  ))

(defun bst-find (obj bst test)
  (if (null bst)
	  nil
	  (let ((elt (node-elt bst)))
		(if (eql obj test)
			bst
		    (if (funcall test obj elt)
				(bst-find obj (node-l bst) test)
			    (bst-find obj (node-r bst) test)
				)))))


;; 終端まで行くとnilとなるので
;; -> (and bst (or nil bst) 
;; -> (and bst bst)
;; -> bst
;; となり bst が返る

(defun bst-min (bst)
  (and bst
	   (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst)
  (and bst
	   (or (bst-max (node-r bst)) bst)))


(setf bst-nums nil)
(dolist (x '(5 8 4 2 1 9 6 7 3))
  (setf bst-nums (bst-insert x bst-nums #'<)))


;;;;------------------------------------
;;;; 4.8 ハッシュテーブル
;;;;------------------------------------

(setf ht (make-hash-table))
;; -> #S(HASH-TABLE :TEST FASTHASH-EQL)

(gethash 'color ht)
;; -> NIL NIL
;;   第1の戻り値はkeyに対応するvalue
;;   第2の戻り値はkeyに対応するvalueをテーブルがもつかどうか

(setf (gethash 'color ht) 'red)
;; -> RED

(gethash 'color ht)
;; -> RED
;; -> T


;;; valueの保持はsetfの他にpushも使える
(set bugs (make-hash-table))

(defun my-member (obj lst)
  (if (null lst)
	  nil
	  (if (eql (car lst) obj)
		  lst
		  (my-member obj (cdr lst)))))

(push "doesn't take keyword arguments."
	  (gethash #'my-member bugs))

(gethash #'my-member bugs)
;; -> ("doesn't take keyword arguments.")
;; -> T


;;; remhash : テーブルからオブジェクトを削除
(remhash 'color ht)
;; -> NIL

;;; maphash : テーブルの全てのkey-valueに対し関数を適用する
(setf (gethash 'color ht) 'blue
	  (gethash 'sharp ht) 'spherical
	  (gethash 'size ht) 'giant)

(maphash #'(lambda (key value)
			 (format t "~A = ~A~%" key value))
		 ht)
;; -> SIZE = GIANT
;; -> SHARP = SPHERICAL
;; -> COLOR = BLUE
;; -> NIL


;; ハッシュテーブルのtest関数の入れ替えも可能

(setf writers (make-hash-table))
(setf (gethash '(ralph waldo emerson) writers) t)
(gethash '(ralph waldo emerson) writers)
;; -> NIL
;;    デフォルトのtest関数はeqlのため

(setf writers2 (make-hash-table :test #'equal))
(setf (gethash '(ralph waldo emerson) writers2) t)
(gethash '(ralph waldo emerson) writers2)
;; -> T



;;;;------------------------------------
;;;; 4.x 練習問題
;;;;------------------------------------

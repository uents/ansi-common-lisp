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
;;;; 4.x 練習問題
;;;;------------------------------------

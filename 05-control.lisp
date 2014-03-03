;;;;
;;;; @brief 5. 制御構造
;;;; @see ANSI Common Lisp
;;;;

;;;;------------------------------------
;;;; 5.1 ブロック
;;;;------------------------------------

;;; progn

;;; block

;;; tagbody


;;;;------------------------------------
;;;; 5.2 コンテキスト
;;;;------------------------------------

;;; let : レキシカルコンテキストを生成

;;; let* 

(let ((x 1)
	  (y (+ x 1)))
  (+ x y))
;; -> letのなかでxを参照できないため、
;;    (+ y (+ x 1)) でエラーする

(let* ((x 1)
	   (y (+ x 1)))
  (+ x y))
;; -> 3


;;;;------------------------------------
;;;; 5.3 条件式
;;;;------------------------------------

;;; if

;;; when

;;; cond

;;; case


;;;;------------------------------------
;;;; 5.4 反復
;;;;------------------------------------

;;; do

;;; do*

;;; dotimes : 0〜nまでn回反復を行う

(dotimes (x 5 x)
  (format t "~A~%" x))
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5

;;; mapc

(mapc #'(lambda (x y)
		  (format t "~A ~A~%" x y))
	  '(hip flip slip)
	  '(hop flop slop))

;; HIP HOP
;; FLIP FLOP
;; SLIP SLOP
;; -> (HIP FLIP SLIP)


;;;;------------------------------------
;;;; 5.5 多値
;;;;------------------------------------

;;; values : 与えられた引数をそのまま多値で返す

(values 'a nil (+ 2 4))
;; -> A
;; -> NIL
;; -> 6

;;; multiple-value-bind : 多値を受け取る

(multiple-value-bind (x y z) (values 1 2 3)
  (list x y z))
;; -> (1 2 3)

(multiple-value-bind (x y z) (values 1 2)
  (list x y z))
;; -> (1 2 NIL)

(multiple-value-bind (s m h D H Y) (get-decoded-time)
  (format nil "~A/~A/~A ~A:~A:~A" Y H D h m s))
;; -> "2014/2/23 2:39:15"

;;; multiple-value-call : 多値を引数として他の関数に渡す

(multiple-value-call #'+ (values 1 2 3))
;; -> 6

;;; multiple-value-list : #'listを第1引数としてmultivple-value-callを呼ぶと等価

(multiple-value-list (values 1 2 3))
;; -> (1 2 3)


;;;;------------------------------------
;;;; 5.6 割り込み
;;;;------------------------------------

;;; catch, throw :
;;; - catchの本体の式はprognと同様順番に評価される
;;; - ただしマッチするタグを持ったthrowが現れると
;;;   catch式は直ちにその結果を返す

(defun super ()
  (catch 'abort
	(sub)
	(format t "we'll never see this.")))

(defun sub ()
  (throw 'abort 99))

(super)
;; -> 99


;;; error :
;;; エラーハンドラ (エラー対処用に特別に用意されている機構) へ遷移する

(progn
  (error "oops! i did again.") ;; -> ここでエラー発生
  (format t ("after the error.")))

;;; unwind-protect :
;;; throwやerrorのような割り込みを無効にする


		 
;;;;------------------------------------
;;;; 5.7 例：日付計算
;;;;------------------------------------

(defconstant month
  #(0 31 59 90 120 151 181 212 243 273 304 334 365)) ;; ベクター(1次元配列)

(defconstant yzero 2000)

(defun leap? (y)
  (and (zerop (mod y 4))
	   (or (zerop (mod y 400))
		   (not (zerop (mod y 100))))))

(defun year-days (y)
  (if (leap? y) 366 365))

;;; 日付を日数に変換
(defun date->num (d m y)
  (+ (- d 1) (month-num m y) (year-num y)))

;;; その月の開始前までの日数を返す
(defun month-num (m y)
  (+ (svref month (- m 1))
	 (if (and (> m 2) (leap? y))
		 1 ;; 閏年かつ2月以降ならさらに1日追加
	     0)
	 ))

;;; その年の1/1に対応する日数を返す
;;; (2000/1/1なら0)
(defun year-num (y)
  (let ((d 0))
	(if (>= y yzero)
		(dotimes (i (- y yzero) d)
		  (incf d (year-days (+ yzero i))))
	    (dotimes (i (- y yzero) (- d))
		  (incf d (year-days (+ y i))))
		)))

;;; 日数を日付に変換
(defun num->date (n)
  (multiple-value-bind (y rest) (num-year n)
	(multiple-value-bind (m d) (num-month rest y)
	  (values d m y))))

;;; 日数を年と余りの日数に変換
(defun num-year (n)
  (if (< n 0)
	  (do* ((y (- yzero i) (- y 1))
			(d (- (year-days y)) (- d (year-days y))))
		  ((<= d n) (values y (- n d))))
	  (do* ((y yzero (+ y 1))
			(prev 0 d)
			(d (year-days y) (+ d (year-days y))))
		  ((> d n) (values y (- n prev))))
	  ))

;;; 日数を月と日に変換
(defun num-month (n y)
  (if (leap? y)
	  (cond ((= n 59) (values 2 29))
			((> n 59) (nmon (- n 1)))
			(t (nmon n)))
	  (nmon n)
	  ))

(defun nmon (n)
  (let ((m (position n month :test #'<)))
	(values m (+ 1 (- n (svref month (- m 1)))))
	))

;;; 日付に日数を加えた時の日付を求める
(defun date+ (d m y n)
  (num->date (+ (date->num d m y) n)))



;;;;------------------------------------
;;;; 5.x 練習問題
;;;;------------------------------------


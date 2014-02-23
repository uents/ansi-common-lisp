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





;;;;------------------------------------
;;;; 5.x 練習問題
;;;;------------------------------------


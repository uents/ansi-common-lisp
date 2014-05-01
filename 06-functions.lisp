;;;;
;;;; @brief 6. 制御構造
;;;; @see ANSI Common Lisp
;;;;

;;;;------------------------------------
;;;; 6.1 大域関数
;;;;------------------------------------

;;; fboundp : 指定したシンボルの関数を探索
(fboundp '+) ; => t

;;; setfで新たな大域関数を定義
(setf (symbol-function 'add2)
	  #'(lambda (x) (+ x 2)))
(fboundp 'add2) ; => t

;;; defunでも同じ
(defun add3 (x) (+ x 2))
(fboundp 'add3) ; => t

(defun primo (lst) (car lst))
(defun (setf primo) (val lst)
  (setf (car lst) val))

(let ((x (list 'a 'b 'c)))
  (setf (primo x) 480) ;; -> 上記のprimo関数の2つ目が呼ばれる
  x)


;;;;------------------------------------
;;;; 6.2 局所関数
;;;;------------------------------------


;;;;------------------------------------
;;;; 6.3 パラメータリスト
;;;;------------------------------------


;;;;------------------------------------
;;;; 6.4 ユーティリティ
;;;;------------------------------------


;;;;------------------------------------
;;;; 6.5 クロージャ
;;;;------------------------------------

;;;;------------------------------------
;;;; 6.6 関数ビルダ
;;;;------------------------------------

(defun compose-simple (f g)
  (lambda (x) (funcall f (funcall g x))))

(defun compose (&rest funcs)
  (destructuring-bind (func1 . rest_funcs) (reverse funcs)
	#'(lambda (&rest args)
		(reduce #'(lambda (accum func) (funcall func accum))
				rest_funcs
				:initial-value (apply func1 args)))))

;; destructuring-bind を使わない方がシンプルに書ける
(defun compose-ex (func &rest funcs)
  (if (null funcs)
	  func
	  #'(lambda (&rest args)
		  (reduce #'(lambda (accum f) (funcall f accum))
				  funcs
				  :initial-value (apply func args)))))

(defun disjoin (func &rest funcs)
  (if (null funcs)
	  func
	  (let ((disj (apply #'disjoin funcs)))
		#'(lambda (&rest args)
			(or (apply func args) (apply disj args))))))

;; -> 再帰呼び出しで展開され
;;  (or (apply f1 args) (or (apply f2 args) (or (apply f3 args) (apply f4 args))))
;; のような形で評価される

(defun conjoin (func &rest funcs)
  (if (null funcs)
	  func
	  (let ((conj (apply #'conjoin funcs)))
		#'(lambda (&rest args)
			(and (apply func args) (apply conj args))))))



;;;;------------------------------------
;;;; 6.7 動的スコープ
;;;;------------------------------------


;;;;------------------------------------
;;;; 6.8 コンパイル
;;;;------------------------------------


;;;;------------------------------------
;;;; 6.9 関数の利用
;;;;------------------------------------


;;;;------------------------------------
;;;; 6.x 練習問題
;;;;------------------------------------

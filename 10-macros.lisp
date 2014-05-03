;;;;
;;;; @brief 10. マクロ
;;;; @see ANSI Common Lisp
;;;;

;;;;------------------------------------
;;;; 10.2 マクロ
;;;;------------------------------------

(defmacro nil! (x)
  (list 'setf x nil))

;; (nil x) => NIL
;; x => NIL

(macroexpand-1 '(nil! x))
;; (SETF X NIL)
;; => T

(funcall (lambda (expr)
		   (apply #'(lambda (x) (list 'setf x nil))
				  (cdr expr)))
		 '(nil! a))
;; => (SETF A NIL)
;; a => NIL
;; defmacroを使うことは上記のような関数を定義しているに等しい



(funcall (lambda (expr)
		   (apply #'(lambda (x) (list 'setf x nil))
				  (cdr expr)))
		 '(nil! a))
;; => (SETF A NIL)
;; a => NIL
;; defmacroを使うことは上記のような関数を定義しているに等しい


;;;;------------------------------------
;;;; 10.3 バッククォート
;;;;------------------------------------

;;; バッククォート：ひな形からリストを作成できる

`(a b c)
;; => (A B C)
;; '(a b c) や (list 'a 'b 'c) と同じ結果

;;; ,をつかうと部分的に評価できる

(setf a 1 b 2)
`(a is ,a and b is ,b)
;; => (A IS 1 AND B IS 2)

(setf first-name 'Steave family-name 'Jobs)
`(his name is ,first-name ,family-name)
;; => (HIS NAME IS STEAVE JOBS)

;; 数値とシンボルでは上手く行く。文字列では？


;;; ,@はリストをスライスする

(setf lst '(a b c))
`(lst is ,lst)  ;; => (LIST IS (A B C))
`(lst is ,@lst) ;; => (LIST IS A B C)


;;; ,@はレストパラメータの展開で役立つ

(let ((x 0))
  (while (< x 10)
	(format t "~A ~%" x)
	(incf x)
	))

(defmacro while (test &rest body)
  `(do ()
	   ((not ,test))
	 ,@body))


;;;;------------------------------------
;;;; 10.4 例：クイックソート
;;;;------------------------------------

;;; rotatef : 参照先の入れ替え
(setf a 1 b 2)
(rotatef a b)
;; a => 2
;; b => 1

;;; クイックソート
;;; 1. 中央の要素をピボットとして選ぶ
;;; 2. ベクトルを区画に分ける
;;;    ピボット未満の要素はすべてピボット以上の要素の左方にくるようにする
;;; 3. 複数の要素がある区画があれば、ベクトルのその部分に対して再帰的に適用
;;;
;;; rotatefを使っているので破壊的

(defun quicksort (vec l r)
  (let ((i l)
		(j r)
		(pivot (svref vec (round (+ l r) 2))))  ; 1
	(while (<= i j)                             ; 2
	  (while (< (svref vec i) pivot) (incf i))
	  (while (> (svref vec j) pivot) (decf j))
	  (when (<= i j)
		(rotatef (svref vec i) (svref vec j))
		(incf i)
		(decf j)))
	(if (>= (- j l) 1) (quicksort vec l j))     ; 3
	(if (>= (- r i) 1) (quicksort vec i r))
	))

;;; もっとシンプルに書けない？
(setf vec (make-array 10))
(map-into vec #'(lambda (x) (random 100)) vec)

;;; ソート実行
(quicksort vec 0 (- (length vec) 1))



;;;;------------------------------------
;;;; 10.5 マクロ設計
;;;;------------------------------------

;;; 悪い例１：xという名前の変数が既に存在すると
;;; 予想外の動作をする
(defmacro ntimes-wrong1 (n &rest body)
  `(do ((x 0 (+ x 1)))
	   ((>= x ,n))
	 ,@body))

(let ((x 10))
  (ntimes-wrong1 5
		  (setf x (+ x 1)))
  x)
;; => 10 (15ではない)


;;; 悪い例２：
(defmacro ntimes-wrong2 (n &rest body)
  (let ((g (gensym)))
	`(do ((,g 0 (+ ,g 1)))
		 ((>= ,g ,n))
	   ,@body)))

(let ((x 10))
  (ntimes-wrong2 (setf x (- x 1))
			   (princ ".")))

;; => .....NIL (.の数が9ではなく5)
;;  (setf x (- x 1)) がマクロ展開されるため、doの終了条件が、
;; (>= ,g 9)
;; (>= ,g 8)
;; (>= ,g 7)
;; (>= ,g 6)
;; (>= ,g 5)
;; と1回のループ毎に変わって行く


;;; 良い例：
(defmacro ntimes-good (n &rest body)
  (let ((g (gensym))
		(h (gensym)))
	`(let ((,h ,n))
	   (do ((,g 0 (+ ,g 1)))
		   ((>= ,g ,h))
		 ,@body))))

(let ((x 10))
  (ntimes-good (setf x (- x 1))
			   (princ ".")))

;; => .........NIL (正解)


;;; 組み込みマクロの展開はmacroexpand-1と
;;; pprint (pretty printの略) の組み合わせが便利

(pprint (macroexpand-1 `(cond (a b)
							  (c d e)
							  (t f))))
;; => (IF A B (IF C (PROGN D E) F))



;;;;------------------------------------
;;;; 10.6 一般化参照
;;;;------------------------------------

(setf lst nil)
(setf (car (push 1 lst)) (1+ (car (push 1 lst))))
;; => 2
;; lst
;; => (1 2)

(defmacro incf-wrong (x &optional (y 1))
  `(setf ,x (+ ,x ,y)))

(incf-wrong (car (push 1 lst)))
;; => 2
;; lst
;; => (1 2 1 2) [(2 1 2)ではない]

(define-modify-macro incf-good (&optional (y 1)) +)

(incf-good (car (push 1 lst)))
;; => 2
;; lst
;; => (2 1 2)


;;; append1f : pushの変形版

(define-modify-macro append1f (val)
  (lambda (lst val) (append lst (list val))))

(let ((l '(a b c)))
  (append1f l 'd)
  l)
;; => (A B C D)


;;;;------------------------------------
;;;; 10.7 例：マクロユーティリティー
;;;;------------------------------------

(defmacro for (var start end &body body)
  (let ((ed (gensym)))
	`(do ((,var ,start (1+ ,var))
		  (,ed ,end))
		 ((> ,var ,ed))
	   ,@body)))

(for x 1 8
	 (princ x))
;; 12345678
;; => NIL

(defmacro in (obj &rest choices)
  (let ((in_ (gensym)))
	`(let ((,in_ ,obj))
	   (or ,@(mapcar #'(lambda (c) `(eql ,in_ ,c))
					 choices)))))

(setf expr '(+ 1 2))
(in (car expr) '+ '- '*)
;; => T
;;
;; ,@のおかげでmapcarの結果である(T NIL NIL)を展開できるため
;; より簡潔に実装できる。
;;
;; マクロを使わない場合は以下のように実装しなければならない
;; (let ((op (car expr)))
;;   (or (eql op '+)
;;       (eql op '-)
;;       (eql op '*)))


(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
	 ,@(let ((key -1))
		 (mapcar #'(lambda (expr) `(,(incf key) ,expr))
				 exprs))))

(random-choice '(turn-left) '(turn-right))
;; => (TURN-LEFT) or (TURN-RIGHT)
;;
;; マクロを使わない場合
;; (case (random 2)
;;   (0 (turn-left))
;;   (1 (turn-right)))


(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(defun avg2 (&rest args)
  (/ (apply #'+ args) (length args)))

(avg 1 2 3 4)  ;; => 5/2
(avg2 1 2 3 4) ;; => 5/2

;; 関数版はapplyを使わなければならないため、
;; 実行時に引数の数を調べる必要はある.
;; マクロ版はコンパイル時にそれを済ませることが
;; できるため関数版より速い


;;;;------------------------------------
;;;; 10.x 練習問題
;;;;------------------------------------

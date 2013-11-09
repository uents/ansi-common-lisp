;;;;
;;;; @brief 3. リスト
;;;; @see ANSI Common Lisp
;;;;

;;;;------------------------------------
;;;; 3.1 コンス
;;;;------------------------------------

;;; consp : 引数がコンスの場合にtを返す
(consp nil)           ;; ==> nil
(consp 1)             ;; ==> nil
(consp '(1))          ;; ==> t
(consp (list 1 2 3))  ;; ==> listのconsを作るので当然t

;;; アトム以外はtを返すlistpは以下のように実装可能
(defun my-listp (lst)
  (or (null lst) (listp lst)))

;;; atom : アトムならtを返す
(atom nil)           ;; ==> t
(atom 1)             ;; ==> t
(atom '(1))          ;; ==> nil
(atom (list 1 2 3))  ;; ==> nil

;;; アトムの検出は以下で実装可能
(defun my-atom (x)
  (not (consp x)))


;;;;------------------------------------
;;;; 3.2 リストにおける等値
;;;;------------------------------------

;;; eql : 同じオブジェクトならTを返す

(setf x (list 1 2 3))
(setf y x) ;; shallow copy (単なる参照渡し)
(eql x y) ;; ==> T

(eql (cons 'a nil) (cons 'a nil)) ;; ==> 別のオブジェクトの比較なのでNIL


;;; equal : オブジェクトの中身が同じならTを返す

(equal x y) ;; ==> T
(equal (cons 'a nil) (cons 'a nil)) ;; ==> T


;;;;------------------------------------
;;;; 3.3 なぜLispにポインタがないのか
;;;;------------------------------------


;;;;------------------------------------
;;;; 3.4 リストをつくる
;;;;------------------------------------

;;; copy-list : リストをコピーする
(setf x (cons 'a nil))
(setf y (copy-list x))

(equal x y) ;; ==> T
(eql   x y) ;; ==> NIL

(defun my-copy-list (lst)
  (if (atom lst)
	  lst
	  (cons (car lst)
			(my-copy-list (cdr lst)))))

;;; append : リストを連結する
(append '(a b) (list 1 2 3) (list 'c 'd)) ;; ==> (A B 1 2 3 C D)


;;;;------------------------------------
;;;; 3.5 例：圧縮
;;;;------------------------------------

;;; (compress '(1 1 1 0 1 0 0 0 0 1))
;;; ==> ((3 1) 0 1 (4 0) 1))

;;; (uncompress '((3 1) 0 1 (4 0) 1))
;;; ==> (1 1 1 0 1 0 0 0 1)


;;; 上級のプログラマーなら書かないだろう例
;;; いったんパス...


;;;;------------------------------------
;;;; 3.6 アクセス関数
;;;;------------------------------------

;;; nth : リストの任意の位置の要素を引き出す
(nth 0 '(a b c))      ;; ==> (A)
(nth 0 '((a b) c d))  ;; ==> (B)

;;; nthcdr : 任意の位置からcdr要素を引き出す
(nthcdr 0 '((a b) c d)) ;; ==> ((A B) C D)
(nthcdr 1 '((a b) c d)) ;; ==> (C D)
(nthcdr 2 '((a b) c d)) ;; ==> (D)
(nthcdr 3 '((a b) c d)) ;; ==> NIL

(defun my-nthcdr (n lst)
  (if (zerop n) ;; 0ならTを返す
	  lst
	  (my-nthcdr (- n 1) (cdr lst))))


;;; cadr とか caddr とか cddar とかあるけど
;;; 読みにくいので使うのは避けよう


;;;;------------------------------------
;;;; 3.7 マッピング関数
;;;;------------------------------------

;;; mapcar : リストの要素に対して連続的に関数を適用

(mapcar #'(lambda (x) (+ x 10))
		(list 1 2 3))

(mapcar #'list
		'(a b c)
		'(1 2 3)) ;; ==> ((A 1) (B 2) (C 3))


;;; maplist : cdr部を順次取り出し関数を適用

(maplist #'(lambda (x) (apply #'+ x))
		 '(1 2 3 4)) ;; ==> (10 9 7 4)


;;;;------------------------------------
;;;; 3.8 ツリー
;;;;------------------------------------

;;;; cons は二分木(binary tree)に見立てることもできる

(defun my-copy-tree (tree)
  (if (atom tree)
	  tree
	  (cons (my-copy-tree (car tree))     ;; my-copy-listでは単に (car tree) だった
			(my-copy-tree (cdr tree)))))

(setf x '(a (b c) d))
(setf y (my-copy-list x))  ;; ==> (A (B C) D)
(setf z (my-copy-tree x))  ;; ==> (A (B C) D)


;;; substitute : リストの要素を置換する

(substitute 'y 'x '(w x y z))   ;; ==> (W Y Y Z)
(substitute 'y 'x '(w (x y) z)) ;; ==> (W (X Y) Z)
                                ;;     リストの中のリストはみないため上手く行かない

;;; subst : ツリーを探索して要素を置換する。

(subst 'y 'x '(w x y z))   ;; ==> (W Y Y Z)
(subst 'y 'x '(w (x y) z)) ;; ==> (W (Y Y) Z)

(defun my-subst (new old tree)
  (if (eql old tree)
	  new
	  (my-copy-tree tree)))

(my-subst 'y 'x '(w x y z))   ;; ==> (W Y Y Z)
(my-subst 'y 'x '(w (x y) z)) ;; ==> (W (Y Y) Z)


;;;;------------------------------------
;;;; 3.9 再帰の理解
;;;;------------------------------------

;;; 帰納方的に考える
;;;  1. 長さ0のリストでうまくゆく
;;;  2. 長さnのリストでうまくゆくなら、n+1のリストでもうまくゆく

;;; 実装時は終了条件を忘れずに
;;; nullのテストを行うことで保証する


;;;;------------------------------------
;;;; 3.10 集合
;;;;------------------------------------

;;; member : 一致した要素以降のオブジェクトを返す

(member 'b '(a b c)) ;; ==> (B C)

;;; testキーワードを使うと、比較条件を変更できる

(member 3 '(1 2 4 5))           ;; ==> NIL
(member 3 '(1 2 4 5) :test #'<) ;; ==> (4 5)
(member 3 '(1 2 4 5) :test #'>) ;; ==> (1 2 4 5)

;;; keyキーワードを使うと、比較前に各要素に適用する

(member 3 '(1 2 4 5) :test #'>) ;; ==> (1 2 4 5)
(member 3 '(1 2 4 5) :test #'>
		             :key #'(lambda (x) (* x 10))) ;; ==> NIL


;;; member-if : 述語を任意に設定できる

(member-if #'oddp '(2 3 4 5))   ;; ==> (3 4 5)

;; member-ifの帰納限定版
(defun my-member-if (func lst)
  (and (consp lst)
	   (if (funcall func (car lst))
		   lst
		   (my-member-if func (cdr lst)))))



;;;;------------------------------------
;;;; 3.x 
;;;;------------------------------------




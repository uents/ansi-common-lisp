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
	  (cons (car lst) (my-copy-list (cdr lst)))))

;;; append : リストを連結する
(append '(a b) (list 1 2 3) (list 'c 'd)) ;; ==> (A B 1 2 3 C D)







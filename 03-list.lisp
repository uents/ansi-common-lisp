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
;;;; 3.11 シーケンス
;;;;------------------------------------

;;; シーケンス (sequence)
;;;  - オブジェクトが特定の順序で並んでいるもの
;;;  - Common Lispではリストとベクタをあわせてシーケンスと呼ぶ

;;; length : シーケンスの長さを返す
(length '(a b c))

;;; subseq : シーケンスの一部をコピーする
(subseq '(a b c d) 1 2) ;; ==> (B)
(subseq '(a b c d) 1)   ;; ==> (B C D)

;;; reverse : シーケンスの並びを逆転させる
(reverse '(a b c))  ;; ==> (C B A)

;;; 回文かどうかのチェック関数
(defun mirror? (s)
  (let ((len (length s)))
	(and (evenp len)                          ;; 文字数が偶数かつ
		 (let ((mid (/ len 2)))
		   (equal (subseq s 0 mid)            ;; 文字列の前半と
				  (reverse (subseq s mid)))   ;; 文字列の後半の転置が一致
		   ))))

;;; sort : 比較関数に従ってソートする
;;;        破壊的な関数なので注意

(setf lst '(0 2 1 3 8))
(sort lst #'>)  ;; ==> (8 3 2 1 0)
lst  ;; ==> (8 3 2 1 0)

(defun nthmost (n lst) ;; n番目に大きい要素を返す
  (nth (- n 1)
	   (sort (copy-list lst) #'>)))

(setf lst '(0 2 1 3 8))
(nthmost 2 lst) ;; ==> 3
lst ;; ==> (0 2 1 3 8)
    ;;     nthmostの中でlstはコピーされるため破壊されていない


;;; every, some :
;;;  - 引数のシーケンスが１つの場合
;;;     述語の引数にシーケンスの各要素を与える
;;;  - 引数のシーケンスが２つの場合
;;;     述語の引数に全てのシーケンスから１つずつ要素を与える

(every #'oddp '(1 3 5))  ;; ==> T
(every #'oddp '(1 2 3))  ;; ==> NIL
(some #'oddp '(1 3 5))   ;; ==> T
(some #'oddp '(1 2 3))   ;; ==> T

(every #'> '(10 11 12) '(1 2 3))  ;; ==> T


;;;;------------------------------------
;;;; 3.12 スタック
;;;;------------------------------------

;;; push, pop : リストの前方へ/から要素を加える/取り出す

(setf lst '(b c))
(push 'a lst)  ;; ==> (A B C)
(pop lst)      ;; ==> A

(defun my-push (obj lst)
  (setf lst (cons obj lst))) ;; これではlstを更新できない
                             ;; (更新するやり方がわからず..)

(defun my-pop (lst)
  (let ((x (car lst)))
	(setf lst (cdr lst)) ;; これではlstを破壊できない
                         ;; (破壊するやり方がわからず..)
	x))

(defun my-reverse (lst)
  (let ((acc nil)) ; accはaccumelationの略かな
	(dolist (obj lst)
	  (push obj acc))
	acc))

;; pushnew : pushの変形版。要素の追加にconsではなくadjoinを使う
(setf x '(a b)) 
(pushnew 'c x)   ;; ==> (C A B)
(pushnew 'a x)   ;; ==> (C A B)のまま (aはもうリストメンバなのでpushされない)


;;;;------------------------------------
;;;; 3.13 ドットリスト
;;;;------------------------------------

;;; 真リスト : nilかcdr部が真リストであるコンス
;;;            listで作られるリストは常に真リスト

(defun proper-list? (x)
  (or (null x)
	  (and (consp x)
		   (proper-list? (cdr x)))))

(proper-list? (list 1 2 3))            ;; ==> T
(proper-list? (cons 'a (cons 'b nil))) ;; ==> T
(proper-list? (cons 'a (cons 'b 'c)))  ;; ==> NIL


;;; ドットリスト : 真リストでないコンス
;;;                car部とcdr部がドットで分離表示される

(cons 'a 'b)                    ;; ==> (A . B)
(cons '(x y) 'a)                ;; ==> ((X Y) . (A B))
(cons '(x y) '(a b))            ;; ==> ((X Y) A B)
(cons '(x y) (cons '(a b) 'c))  ;; ==> ((X Y) (A B) . C)

; 単純にドットでも書ける
'(a . b)  ;; ==> (A . B)


;;;;------------------------------------
;;;; 3.14 連想リスト
;;;;------------------------------------

;;; コンスのリストは連想リスト(assoc-list) またはalistと呼ばれる
(setf trans '((+ . "add") (- . "substract"))) ;; ==> ((+ . "add") (- . "substract"))

; 上記はこう書くのと同じ
(setf trans2 (list (cons '+ "add") (cons '- "substract")))

;;; assoc : 指定されたキーからペアを引き出す
(assoc '+ trans) ;; ==> (+ . "add")

(defun my-assoc (key alist)
  (and (consp alist)
	   (let ((pair (car alist)))
		 (if (eql key (car pair))
			 pair
		     (my-assoc key (cdr alist))))))

(my-assoc '+ trans)  ;; ==> (+ . "add")
(my-assoc '* trans)  ;; ==> NIL


;;;;------------------------------------
;;;; 3.15 例：最短経路
;;;;------------------------------------

;;; shortest-path 最短経路を返す
;;;  start : 出発ノード
;;;  end   : 目的地ノード
;;;  net   : ネットワーク

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (print queue)
  (if (null queue)
	  nil
	  (let ((path (car queue)))
		(let ((node (car path)))
		  (if (eql node end)
			  (reverse path)
			  (bfs end
				   (append (cdr queue)
						   (new-paths path node net))
				   net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
			  (cons n path))
		  (cdr (assoc node net))))



;;;;------------------------------------
;;;; 3.x 練習問題
;;;;------------------------------------

;;; 2. もとのリストでの要素の並びを保持するように動作する
;;;    unionの変形版を書け

;; unionを使うと順序が保証できないのでappendで連結する
(defun new-union (x y)
  (if (null y)
	  x
	  (new-union (if (member (car y) x)
					 x
					 (append x (list (car y))))
				 (cdr y))))



;;; 3. １つのリストを引数として、おのおのの要素について同じもの（eqlで比較）
;;;    が出現する回数を示すリストを返す関数を定義せよ

(defun occurences-base (result elt rest)
;  (format t "~A ~A ~A ~%" result elt rest)
  (if (null elt)
	  result
	  (let ((pair (assoc elt result)))
		(if (null pair)
			(progn
			  (setf pair (cons elt 1))
			  (setf result (append result (list pair)))
			  )
		    (setf (cdr pair) (+ (cdr pair) 1)))
		(occurences-base result (car rest) (cdr rest)))))

(defun occurences (lst)
  (sort (occurences-base nil (car lst) (cdr lst)) #'> :key #'cdr))


;;; 4. なぜ (member '(a) '((a) (b))) は nil を返すのか？
;;;
;;; - 比較関数がeqlのため、異なるオブジェクトは偽となる
;;; - tを返すようにするには、:test にequalを指定する


;;; 5. 関数pos+は1つのリストを引数として、おのおのの要素にその位置を示す数を加えて返す

;;; (a) 再帰版

(defun pos+-iter-base (result pos rest)
;  (format t "~A ~A ~A ~%" result pos rest)
  (if (null rest)
	  result
	  (progn
		(let ((elt (list (+ pos (car rest)))))
		  (setf result (append result elt)))
		(pos+-iter-base result (+ pos 1) (cdr rest)))))

(defun pos+-iter (lst)
  (pos+-iter-base nil 0 lst))


;;; (b) 反復版

(defun pos+-recur-base (result pos rest)
  (dolist (obj rest)
	(progn
	  (setf result (append result (list (+ pos obj))))
	  (setf pos (+ pos 1))
;	  (format t "~A ~A ~A ~%" result pos obj)
	  ))
  result)

(defun pos+-recur (lst)
  (pos+-recur-base nil 0 lst))


;;; (c) mapcarを用いる版

(defun pos+-map (lst)
  (let ((pos -1))
	(mapcar #'(lambda (x) (progn (setf pos (+ pos 1)) (+ x pos)))
			lst)))


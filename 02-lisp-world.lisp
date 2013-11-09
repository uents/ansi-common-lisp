;;;
;;; @brief 2. Lispの世界へようこそ 
;;; @see ANSI Common Lisp
;;;

;;;;------------------------------------
;;;; 2.1 式
;;;;------------------------------------

;;; 全てのLispの式は、アトムかリスト

;;; - アトム： 数値, 真偽値(t/nil)
;;; - リスト： 0個以上の式を括弧でくくったもの


;;;;------------------------------------
;;;; 2.2 評価
;;;;------------------------------------

;;;;------------------------------------
;;;; 2.3 データ
;;;;------------------------------------

;;; Lispで扱うデータ型

;;; 整数    1

;;; 文字列  "hello world"

;;; シンボル
;;;  'hello
;;;    シンボルを参照したい時はクォートする必要がある

;;; リスト
;;;  '(hello world)
;;;    ()の前にクォートをつける
;;;  (list 'hello 'world)
;;;    またはlist関数をつける


;;; それ以外のデータ型は後の章で...


;;;;------------------------------------
;;;; 2.4 リストオペレータ
;;;;------------------------------------

;;; cons : リストを作る関数
(cons 'a '(b c d)) ;; ==> (A B C D)

;;; どちらも (A B) となる
(cons 'a (cons 'b nil))
(list 'a 'b)


;;;;------------------------------------
;;;; 2.5 真
;;;;------------------------------------


;;; nil : 空リストのこと。真偽値の偽としても使われる

;; 以下はすべてnil
(null nil)    ;; ==> t
(null 'nil)   ;; ==> t
(null ())     ;; ==> t
(null '())    ;; ==> t


;;; Lispでは真偽を判別する関数を述語(predicate)と呼ぶ

;;; listp : リストに対して真を返す
(listp nil)      ;; ==> nilはt
(listp 27)       ;; ==> アトムはnil
(listp '(1 2 3)) ;; ==> リストはt

;;; null : nilに対して真を返す
(null nil)       ;; ==> nilはt
(null 27)        ;; ==> アトムはnil
(null '(1 2 3))  ;; ==> リストはt

;;; not : 偽である引数に対して真を返す
(not nil)  ;; ==> T


;;; if : テスト式、then式、else式の3つを引数に取り
;;;      テスト式の真偽に応じてthen/else式を評価しその値を返す
(if (listp '(1 2 3))
	'(i am list)
    '(i am not list)) ;; ==> I AM LIST

;;; テスト式の真偽とはnilなら偽、それ以外は真である
(if nil 1 -1)   ;; ==> -1
(if t 1 -1)     ;; ==> 1
(if 0 1 -1)     ;; ==> 1
(if 'z 1 -1)    ;; ==> 1
(if '(1) 1 -1)  ;; ==> 1


;;; and : 全ての引数が真の場合、最後の引数を返す
;;; or  : 真となる引数を見つけた時点で、その引数を返す (残りの引数は評価しない)
(and t (+ 1 2) (+ 3 4))  ;; ==> 7
(or nil (- 1 2) (/ 1 0)) ;; ==> -1 ((/ 1 0)は評価されない)


;;;;------------------------------------
;;;; 2.6 関数
;;;;------------------------------------

;;; defunで新しい関数を定義できる
(defun my-third (lst)
  (car (cdr (cdr lst))))


;;;;------------------------------------
;;;; 2.7 再帰
;;;;------------------------------------

(defun my-member (obj lst)
  (if (null lst)
	  nil
	  (if (eql (car lst) obj)
		  lst
		  (my-member obj (cdr lst)))))

(my-member 'b '(a b c)) ;; ==> (B C)
(my-member 'z '(a b c)) ;; ==> NIL


;;;;------------------------------------
;;;; 2.8 Lispのコードの読み書き
;;;;------------------------------------

;;;;------------------------------------
;;;; 2.9 入出力
;;;;------------------------------------

;;; format : printfライクな出力関数
(format t "~A plus ~A equals ~A. ~%" 2 3 (+ 2 3))

;;; read : プロンプトを表示し入力されたものを返す関数
(defun askem (string)
  (format t "~A" string)
  (read))


;;;;------------------------------------
;;;; 2.10 変数
;;;;------------------------------------

;;; let : ローカル変数を導入
(defun ask-number ()
  (format t "please enter a number : ")
  (let ((value (read))) ; valueはローカル変数
	(if (numberp value)
		value
	    (ask-number))))

;;; defparameter : グローバル変数を導入
;; 必ずしも * * をつける必要はないが、Lispの慣習としてつける
(defparameter *glob* 99)

;;; defconstant : グローバル定数を定義
(defconstant limit (+ 1 *glob*))

;;; boundp : シンボルがグローバル変数/定数で使われているかチェック
(boundp '*glob*)


;;;;------------------------------------
;;;; 2.11 代入
;;;;------------------------------------

;;; setf : いずれの種類の変数へも代入できる

(setf *glob* 98) ; ==> 98

(let ((n 10))
  (setf n 2)
  n) ; ==> 2

(setf x (list 'a 'b 'c)) ; ==> (A B C)
                         ; xは暗黙的にグローバル変数とみなされる
                         ; もちろんソースファイル上では、
                         ; 明示的にdefparameterを使う方が望ましい

(set (car x) 'n) ; ==> (N B C)

;; 複数の変数に代入することも可能
(setf a 1
	  b 2
	  c 3)


;;;;------------------------------------
;;;; 2.12 関数プログラミング
;;;;------------------------------------

;;; removeは非破壊的
(setf lst '(c a r a t))
(remove 'a lst) ; ==> (C R T)
lst ; ==> (C A R A T)

;;; 本当にリストを更新したい場合はsetfを使う
(setf lst (remove 'a lst))

;;; remove (に限らず) が非破壊的なのは、関数の副作用を避けるため
;;; (副作用とは、関数の出力以外で変数の変更を行うこと)
;;; これは関数プログラミングにとって重要な概念.


;;;;------------------------------------
;;;; 2.13 反復
;;;;------------------------------------

;;; do : 反復オペレータの代表的マクロ
(defun show-squares-iter (start end)
  (do ((i start (+ i 1)))            ; 第１引数で変数定義と初期値の設定を行う
	  ((> i end) 'done)              ; 第２引数の第１式は反復の終了条件テスト
                                     ; 第２引数の第２式は反復が終了した時に実行される式
	(format t "~A ~A ~%" i (* i i))  ; 残りの引数は反復毎に評価される式
	))

;;; これを再帰で書き直す
(defun show-squares-recur (start end)
  (if (> start end)
	  'done
	  (let ((i start))
		(progn
		  (format t "~A ~A ~%" start (* i i))
		  (show-squares-recur (+ i 1) end)
		  ))))


;;; dolist : リストの要素毎に反復処理を行う
(defun my-length-iter (lst)
  (let ((length 0))
	(dolist (obj lst)                  ; 第１引数で要素を変数に取り出す
	  (setf length (+ 1 length)))      ; 第２引数は要素を取り出す度に実行される式
	length))

;;; これを再帰で書き直す
(defun my-length-recur (lst)
  (if (null lst)
	  0
	  (+ 1 (my-length-recur (cdr lst)))))


;;;;------------------------------------
;;;; 2.14 オブジェクトとしての関数
;;;;------------------------------------

;;; function : 関数名と結びついたオブジェクトを返す

(function my-length-recur)
; ==> #<FUNCTION MY-LENGTH-RECUR (LST) (DECLARE (SYSTEM::IN-DEFUN MY-LENGTH-RECUR))
      (BLOCK MY-LENGTH-RECUR (IF (NULL LST) 0 (+ 1 (MY-LENGTH-RECUR (CDR LST)))))>

(function +)
; ==> #<SYSTEM-FUNCTION +>

;;; #' : functionの省略形

#'my-length-iter
; ==> #<FUNCTION MY-LENGTH-RECUR (LST) (DECLARE (SYSTEM::IN-DEFUN MY-LENGTH-RECUR))
      (BLOCK MY-LENGTH-RECUR (IF (NULL LST) 0 (+ 1 (MY-LENGTH-RECUR (CDR LST)))))>

#'+
; ==> #<SYSTEM-FUNCTION +>


;;; apply : 関数とその関数の引数リストを引数とし、関数の実行結果を返す

(apply #'+ (list 1 2 3))     ; ==> 6
(apply #'+ 1 2 (list 3 4 5)) ; ==> 15 : 最後がリストであれば任意の引数が取れる
(apply #'+ 1 2 3)            ; ==> これはエラー

;;; funcall : applyと同じことを行うが、引数がリストである必要がない

(funcall #'+ 1 2 3) ; ==> 6


;;; lambda : 無名関数を定義する

((lambda (x) (+ x 100)) 1) ; ==> 101 : 無名関数を即時実行
(funcall #'(lambda (x) (+ x 100))
		 1)                ; ==> 101


;;;;------------------------------------
;;;; 2.15 データ型
;;;;------------------------------------

;;; Common Lispでは値は型を持つが、変数は型を持たない

;;; オブジェクトには常に複数の型を持つ
;;;
;;; 例えば 27 という数は
;;; - 固定数 (fixnum)
;;; - 整数 (integer)
;;; - 有理数 (relational)
;;; - 実数 (real)
;;; - 数 (number)
;;; - アトム (atom)
;;; - t 
;;; に当てはまる


;;;;------------------------------------
;;;; 2.16 これから先のこと
;;;;------------------------------------


;;;;------------------------------------
;;;; 練習問題
;;;;------------------------------------

;;; Q.1
(+ (- 5 1) (+ 3 7)) ; ==> 14
(list 1 (+ 2 3))    ; ==> (1 5)
(if (list 1) (+ 1 2) (+ 3 4)) ; ==> 3
(list (and (listp 3) t) (+ 1 2)) ; ==> (NIL 3)

;;; Q.2
;;; (a b c) を返すcons式を3つ示せ

(cons 'a (cons 'b (cons 'c nil)))
(cons 'a '(b c))
(cons 'a (cons 'b '(c)))

;;; Q.3
(defun forth (lst)
  (car (cdr (cdr (cdr lst)))))

(forth '(1st 2nd 3rd 4th 5th)) ;; ==> 4TH
(forth '(1st 2nd 3rd)) ;; ==> NIL

;;; Q.4
(defun larger (a b)
  (if (> a b) a b))

;;; Q.5
(defun enigma (lst)
  (and (not (null lst))           ; lstがNILでなければT
	   (or (null (car lst))       ; lstの先頭の要素がNILまたは
		   (enigma (cdr lst)))))  ; lstの次の要素のリストがTならT, NILならNIL

;; ==> 引数のリストにNILを含まなければTを返す
(enigma '(1 2 3 4))   ;; ==> nil
(enigma '(nil 2 3 4)) ;; ==> t
(enigma '(1 2 nil 4)) ;; ==> t
(enigma '(1 2 3 nil)) ;; ==> t


(defun mystery (x y)
  (if (null y)             ; yがnilなら
	  nil                  ; nilを返す
	  (if (eql (car y) x)  ; yとxが同じなら
		  0                ; 0を返す
		  (let ((z (mystery x (cdr y)))) ; でなければ、次の要素のリストをmysteryに渡し
			                             ; 戻値をzに格納し
			(and z (+ z 1)))))))         ; z+1を返す

;;; ==> 引数yのリスト中でxが出現した位置を返す
;;      もしなければnilを返す

(mystery 'c '(a b c d)) ;; ==> 2
(mystery 'e '(a b c d)) ;; ==> nil


;;; Q.6

;;; (car (x (cdr '(a (b c) d))))
;;; ==> B となるオペレータx は car
(car (car (cdr '(a (b c) d))))

;;; (x 13 (/ 1 0)) が 13
;;; ==> or

(or 13 (/ 1 0))
		   
;;; (x #'list 1 nil) が (1)
;;; ==> apply

(apply #'list 1 nil)


;;; Q.7
;;; 1つのリストを引数とし、そのリストの要素にリストがふくまれる場合に真とする関数
(defun elem-listp (lst)
  (if (null lst)
	  nil
	  (if (listp (car lst))
		  t
		  (elem-listp (cdr lst)))))

(elem-listp nil)  ;; ==> nil
(elem-listp '(1)) ;; ==> nil
(elem-listp '(1 2 3 4))   ;; ==> nil
(elem-listp '(1 (2 3) 4)) ;; ==> t

(defun elem-listp-ex (lst)
  (if lst
	  (or (listp (car lst))
		  (elem-listp-ex (cdr lst))))) ;; 結果は上と同じ


;;; Q.8 反復と再帰の2通りの方法を示せ

;;; (a) 正の整数を引数とし、その数のドット(ピリオド)を表示する

(defun show-dot-iter (num)
  (do ((i 0 (+ i 1))) ((= i num))
	   (format t ".")))

(defun show-dot-recur (num)
  (if (> num 0)
	  (progn
		(format t ".")
		(show-dot-recur (- num 1)))))

;;; (b) １つのリストを引数とし、aというシンボルがいくつあるかを返す

(defun count-symbol-iter (lst)
  (let ((count 0))
	(dolist (obj lst)
	  (if (eql obj 'a)
		  (setf count (+ count 1))))
	count))

(defun count-symbol-recur (lst)
  (if lst
	  (if (eql (car lst) 'a)
		  (+ 1 (count-symbol-recur (cdr lst)))
		  (count-symbol-recur (cdr lst)))
	  0))

(defun count-symbol-recur2 (lst)
  (if lst
	  (+ (if (eql (car lst) 'a) 1 0)
		 (count-symbol-recur (cdr lst)))
	  0))


;;; Q.9 
;;; nilでない要素の合計を返す関数を書こうとしている
;;; 間違いを説明し、正しいプログラムを示せ

;;; (a)
;; removeは非破壊的な関数なので、
;; (apply...) にremoveの戻値を渡せばよい

(defun summit (lst)
  (remove nil lst)
  (apply #'+ lst))

(defun summit-fixed (lst)
  (apply #'+ (remove nil lst)))

(summit-fixed '(1 2 nil 3)) ;; ==> 6	  

;;; (b)
;; 終端条件がない
(defun summit2 (lst)
  (let ((x (car lst)))
	(if (null x)
		(summit2 (cdr lst))
	    (+ x (summit2 (cdr lst))))))

(defun summit2-fixed (lst)
  (if lst
	  (let ((x (car lst)))
		(if (null x)
			(summit2-fixed (cdr lst))
		    (+ x (summit2-fixed (cdr lst)))))
	  0))

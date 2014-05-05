;;;;
;;;; @brief 11. COLS
;;;; @see ANSI Common Lisp
;;;;

;;;;------------------------------------
;;;; 11.2 クラスとインスタンス
;;;;------------------------------------

(defclass rectangle ()
  (height width))

(defclass circle ()
  (radius center))

(defmethod area ((x rectangle))
  (* (slot-value x 'height) (slot-value x 'width)))

(defmethod area ((x circle))
  (* pi (expt (slot-value x 'radius) 2)))

(setf r (make-instance 'rectangle))
(setf (slot-value r 'height) 2
	  (slot-value r 'width) 3)
(area r)
;; => 6


;; 継承の例
;;  colorという別クラスを用意し、
;;  circleとcolorの両方をスーパークラスとする
;;  colored-circleというクラスを作成する

(defclass color ()
  (color))

(defclass colored-circle (circle color)
  ())


;;;;------------------------------------
;;;; 11.3 スロットの属性
;;;;------------------------------------

;;; - :accessorを指定することで
;;;   slot-valueなしでフィールドへアクセスが可能
;;; - :initarg, :initform でフィールドの初期値が設定可能

(defclass circle ()
  ((radius :accessor circle-radius
		   :initarg :radius
		   :initform 1)
   (center :accessor circle-center
		   :initarg :center
		   :initform (cons 0 0))))

(setf c (make-instance 'circle :radius 3))
(circle-radius c)
;; => 3
(circle-center c)
;; => (0 . 0)


;;; :allocation :classでフィールドの一部が
;;; 全てのインスタンスで共有可能となる

(defclass tabloid ()
  ((top-story :accessor tabloid-story
			  :allocation :class)))

(setf daily-blab (make-instance 'tabloid)
	  unsolicated-mail (make-instance 'tabloid))

(setf (tabloid-story daily-blab) 'adultery-of-senetor)
(tabloid-story unsolicated-mail)
;; => ADULTERY-OF-SENETOR



;;;;------------------------------------
;;;; 11.4 スーパークラス
;;;;------------------------------------

(defclass graphic ()
  ((color :accessor graphic-color :initarg :color)
   (visible :accessor graphic-visible :initarg :visible
			:initform t)))

(defclass screen-circle (circle graphic)
  ((color :initform 'purple)))

(setf c (make-instance 'screen-circle))

(graphic-color c)
;; => PURPLE


;;;;------------------------------------
;;;; 11.5 優先度
;;;;------------------------------------


;;;;------------------------------------
;;;; 11.6 総称関数
;;;;------------------------------------

(defmethod combine (x y)
  (list x y))

(combine 'a 'b)
;; => (A B)


(defclass stuff ()
  ((name :accessor name
		 :initarg :name)))

(defclass ice-cream (stuff) ())

(defclass topping (stuff) ())

(defmethod combine ((ic ice-cream) (top topping))
  (format nil "~A ice-cream with ~A topping."
		  (name ic) (name top)))


;;; 対象とするオブジェクトに応じて呼び出されるメソッドが変わる
;;; これを総称関数と呼ぶ

(combine (make-instance 'ice-cream :name 'fig)
		 (make-instance 'topping :name 'treacle))
;; => "FIG ice-cream with TREACLE topping."

(combine 23 'skiddoo)
;; => (23 SKIDDOO)


;;;;------------------------------------
;;;; 11.7 補助メソッド
;;;;------------------------------------

(defclass speaker () ())

(defmethod speak ((s speaker) string)
  (format t "~A" string))

(speak (make-instance 'speaker)
	   "I'm hungry")

;; I'm hungry
;; => NIL




;;;;------------------------------------
;;;; 11.x 練習問題
;;;;------------------------------------

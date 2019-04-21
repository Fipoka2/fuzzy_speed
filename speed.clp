;; Шаблон лингвистической переменной "Скорость"
;; Границы значений - от 0 до 400 км/ч
;; Термы - "Низкая", "Средняя", "Высокая"
(deftemplate speed
    0 400 km/h
    ( (low (0 1) (80 0))
      (medium (60 0) (110 1) (170 0))
      (high (150 0) (400 1))
    )
)

;; Начальные факты
(deffacts init-facts
  (speed-value 100)
)

;; Функция fuzzify
;;      Параметры: ?fztemplate - имя fuzzy шаблона (speed)
;;                 ?value - целое число для фаззификации
;;                 ?delta - допустимая погрешность четкого числа
;;
;; Функция создает нечёткий факт по переданному шаблону. Нечёткое множество представлено
;; треугольной функцией принадлежности c максимумом в точке value и нулём в точках value+delta и value-delta.
;; Также присутствует проверка на выход за границы нечеткого множества speed
;;
(deffunction fuzzify (?fztemplate ?value ?delta)
    (bind ?low (get-u-from ?fztemplate))
    (bind ?high (get-u-to ?fztemplate))

    (if (<= ?value ?low)
    then
      (assert-string (format nil "(%s (%g 1.0) (%g 0.0))" ?fztemplate ?low ?delta))
    else (if (>= ?value ?high)
        then
          (assert-string (format nil "(%s (%g 0.0) (%g 1.0))" ?fztemplate (- ?high ?delta) ?high))
        else
          (assert-string (format nil "(%s (%g 0.0) (%g 1.0) (%g 0.0))" 
            ?fztemplate (max ?low (- ?value ?delta)) ?value (min ?high (+ ?value ?delta)) ))
         )
    )
)

;; Правило, добавляющее тестовое значение при наличии факта speed-value.
;; Также выводит график термов и нечеткого числа speed.
(defrule fuzzy-value-rule
    (speed-value ?value)
    =>
    (printout t "adding test value (speed " ?value " 0.001)..." crlf)
    (bind ?speed-address (fuzzify speed ?value 0.001))
    (printout t "showing plot..." crlf)
    (plot-fuzzy-value t -*+S nil nil 
        (create-fuzzy-value speed low) 
        (create-fuzzy-value speed medium) 
        (create-fuzzy-value speed high) 
        ?speed-address
    )
)

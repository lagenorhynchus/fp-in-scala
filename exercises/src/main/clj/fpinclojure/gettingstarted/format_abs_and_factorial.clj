(ns fpinclojure.gettingstarted.format-abs-and-factorial
  (:require
   [fpinclojure.gettingstarted.my-module :refer [abs factorial format-result]]))

;; Now we can use our general `format-result` function
;; with both `abs` and `factorial`
(defn -main [& _]
  (println (format-result "absolute value" -42 abs))
  (println (format-result "factorial" 7 factorial)))

(comment
  (with-out-str
    (-main))
  )

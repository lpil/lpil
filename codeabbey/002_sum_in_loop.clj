(def input "44
975 597 754 1259 341 696 857 675 659 543 652 1212 1173 167 33 394 1220 1273 571 564 1190 627 235 767 1010 619 685 619 894 1124 193 568 420 937 527 751 333 84 126 982 617 769 894 490")

(apply + (map 
           #(Integer/parseInt %)
           (clojure.string/split
             (last (clojure.string/split input #"\n")) #" ")))

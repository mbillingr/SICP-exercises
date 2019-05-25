
(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)

(rule (grandson ?g ?s)
      (and (father ?f ?g)
           (father ?s ?f)))

(rule (father ?s ?m)
      (or (son ?m ?s)
          (and (wife ?m ?w)
               (son? ?w ?s))))


; We find all pairs of people for which lives-near maches. *Of course* we get
; each pair listed in forward and reverse order.
; (If (a b) matches, (b a) matches too)

; I don't think it's easily possible to avoid these duplicates. One would need
; to impose an order on names (lexical seems reasonable) and select only the
; pair where the order increases.

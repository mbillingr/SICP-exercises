; initial state
(stack)
(state (continue (label fib-done))
       (n 3)
       (val *unassigned*))

; before first recursive call
(stack 3 (label fib-done))
(state (continue (label afterfib-n-1))
       (n 2)
       (val *unassigned*))

; before second recursive call
(stack 2 (label afterfib-n-1) 3 (label fib-done))
(state (continue (label afterfib-n-1))
       (n 1)
       (val *unassigned*))

; before returning from immediate answer
(stack 2 (label afterfib-n-1) 3 (label fib-done))
(state (continue (label afterfib-n-1))
      (n 1)
      (val 1))

; before third recursive call
(stack 1 (label afterfib-n-1) 3 (label fib-done))
(state (continue (label afterfib-n-2))
      (n 0)
      (val 1))

; before returning from first recursive call
(stack 3 (label fib-done))
(state (continue (label afterfib-n-1))
      (n 0)
      (val 1))

; before returning from immediate answer
(stack 1 (label fib-done))
(state (continue (label afterfib-n-2))
      (n 1)
      (val 1))

; before done
(stack)
(state (continue (label fib-done))
      (n 1)
      (val 2))

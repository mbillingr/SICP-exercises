(controller
  sqrt-loop
    (test (op good-enough?) (reg guess) (reg x))
    (branch (label done))
    (assign guess (op improve) (reg guess) (reg x))
    (goto (label sqrt-loop))
  done)

(controller
  sqrt-loop
    (assign y (op *) (reg guess) (reg guess))
    (assign diff (op -) (reg y) (reg x))
    (assign abs-diff (op abs) (reg diff))
    (test (op <) (reg abs-diff) (const 0.001))
    (branch (label done))
    (assign rel (op /) (reg x) (reg guess))
    (assign sum (op +) (reg guess) (reg rel))
    (assign guess (op /) (reg sum) (const 2.0))
    (goto (label sqrt-loop))
  done)

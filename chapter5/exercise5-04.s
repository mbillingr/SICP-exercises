; a) recursive exponentiation
(controller
    (assign continue (label expt-done))
  expt-loop
    (test (op =) (reg n) (const 0))
    (branch (label base-case))
    (save continue)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-expt))
    (goto (label expt-loop))
  after-expt
    (restore continue)
    (assign val (op *) (reg b) (reg val))
    (goto (reg continue))
  base-case
    (assign val (const 1))
    (goto (reg continue))
  expt-done)

; b) iterative exponentiation
(controller
  (assign val (const 1))
  expt-loop
    (test (op =) (reg n) (const 0))
    (branch (label expt-done))
    (assign val (op *) (reg b) (reg val))
    (assign n (op -) (reg n) (const 1))
    (goto (label expt-loop))
  expt-done)

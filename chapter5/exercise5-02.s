(controller
  test-c
    (test (op >) (reg c) (const n))
    (branch (label done))
    (assign p (op *) (reg c) (reg p))  ; might need a temporary register for that
    (assign c (op inc) (reg c))  ; might need another temporary register for that
    (goto (label test-c))
  done)

(import (builtin core)
        (sicp utils))


; Q: changes that must be made to a system in order to add new types or new operations
;
; I will distinguish a few parts where changes may need to be made:
;   1. The client code - where generic operations are combined with data for a purpose.
;   2. The generic operation definitions
;   3a. The type definition
;   3b. Specific operations (that take specific types)

; A) Explicit dispatch
;    Dispatch is performed by generic operations on the argument types.
;    - When adding a new type every generic operation that performs dispatch has to be adjusted to correctly work with that type.
;    - When adding a new operation the respective generic operation has to be written.

; B) Data-directed
;    Dispatch is performed by generic operations on the argument types (using a table that relates procedures to operations and types).
;    - When adding a new type it must set the correct table entries.
;    - When adding a new operation the respective generic table entries must be updated for all applicable types. This likely requires changing that part of the type definitions.

; C) Message-passing
;    Dispatch is performed by data objects on the operations performed on them.
;    - When adding a new type it must implement a dispatcher.
;    - When adding a new operation the dispatchers of all applicable types must be updated.


; Q: Which organization would be most appropriate for a system in which new types must often be added?
;    Which would be most appropriate for a system in which new operations must often be added?
;
;    As summarized above, explicit dispatch seems least intrusive when adding a new operation.
;    Both data-directed and message passing seem least intrusive when adding new types.
;
;    If we often add a new type, we don't want to do changes throughout the existing codebase everytime.
;    Both data-directed and message-passing seem well suited for that because in both
;    approaches a type tells the system how operations work on it.
;
;    If we often add a new operation, only the explicit dispatch does not require updating the code base.
;    We just write the new operation so that it can handle the existing types.
;
;
;    Actually, I'm surprised by that outcome. I had not expected explicit dispatch to
;    come off so well, here.
;    I guess the answer the SICP authors would want to hear is:
;       often add types -> message passing
;       often add ops -> data directed
;
;    The table-based data-directed approach is probably flexible enough to handle
;    either situation, depending on how it is implemented.

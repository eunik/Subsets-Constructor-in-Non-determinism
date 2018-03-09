;;helper functions
(define (flatten l)
  (cond ((null? l)'())
        ((list? (car l))(append (flatten (car l)) (flatten (cdr l))))
        (else (cons (car l) (flatten(cdr l))))))

(define (member? x list)
     (if (null? list) #f
         (if (equal? x (car list)) #t
              (member? x (cdr list)))))   

(define (ormap f xs)
    (cond ((null? xs) #f)
          ((f (car xs)) #t)
          (else (ormap f (cdr xs)))))

;precondition: mylist must be a single transition.
;postcondition: extracts a list of 'from' in a list of only 1 transition.
;(get-from (('q0 'q1 ('a 'q2)))  returns ('q0 'q1).
(define (get-from mylist)
  (cond ((list? (car mylist)) '())
        (else (cons (car mylist) (get-from (cdr mylist))))))

;precondition: mylist will be the whole dfa and state is a 'from' state
;that COULD exist in dfa.
;postcondition: returns #t or #f if 'from' state already exist in dfa.
;sometimes we get a list of 'from's such as (('q0 'q1 ('a 'q2)),
;so we use get-from to get ('q0 'q1).
(define (find-from? mylist state)
  (cond ((null? mylist) #f)
        ;is it a list of 'from'?
        ((list? state)
         (cond((equal? (get-from (car mylist)) state) #t)
              (else (find-from? (cdr mylist) state))))
        ;then it must be an atom
        (else (cond((eq? (caar mylist) state) #t)
                   (else (find-from? (cdr mylist) state))))
        ))

;precondition: mylist must be the whole nfa and state is a from
;state.
;postcondition: returns list of 'to's given a 'from' in nfa.
;(find-to ('q0 ('a 'q1)('b 'q2)) 'q0)  returns (('a 'q1)('b 'q2)).
(define (find-to mylist state)
  (cond ((null? mylist) '())
        ((eq? (caar mylist) state) (cdar mylist))
        (else (find-to (cdr mylist) state))))

;;simple procedures
;precondition: state SHOULD exist.
;postcondition: cdr down dfa to see if a state was tested.
;if state is null since we don't wish to update stack we return #t
(define (exist-in-dfa? dfa state)
  (cond((null? state) #t)
       (else (find-from? dfa state))))

;precondition: stack stores a list of 'to' atoms to test.
;stack: the list of temporary states of a-list and b-list
;postcondition: grabs the next state to test from, given a stack of
;states in a temporary list.
;(get-test ('q1 ('q0 'q2)) returns ('q1).
(define (get-test stack)
   (cond ((null? stack) '())
         (else (car stack))))

(define (pop stack)
  (cond ((null? stack)'())
        (else (cdr stack))))

;precondition: stack stores a list of 'to' atoms to test.
;either a-list or b-list must exist.
;postcondition: returns a new stack after popping the old stack.
(define (update-stack dfa stack a-list b-list)
  (let* ((current-stack (pop stack))
         (nonempty-stack? (not (null? current-stack)))
         (input-a? (not (exist-in-dfa? dfa a-list)))
         (input-b? (not (exist-in-dfa? dfa b-list))))
    (cond((and nonempty-stack? input-a? input-b?) (list current-stack a-list b-list))
         ((and input-a? input-b?) (list a-list  b-list))
         (nonempty-stack? (append current-stack
                           (if input-a? (list a-list) '())
                           (if input-b? (list b-list) '())))
         (else (list(append (if nonempty-stack? current-stack '())
          (if input-a? a-list '())
          (if input-b? b-list '()))))
         )))

;precondition: this will be used by (find-to-states)
;state is an atom from current-state that SHOULD exist in nfa and input COULD exist.
;postcondition: returns the appended ‘to’ states that satisfies the input,
;given a ‘from’ state in nfa.
;(get-to-states nfa 'q0 a) in ('q0 (a 'q0) (b 'q1) (a 'q2)) returns ('q0 'q2)
(define (get-to-states nfa state input)
  ;use find-to to get 'to' list
  (define (get-help mylist)
    (cond ((null? mylist)'())
          ;appends 'to' list that satisfies input
          ((eq? (caar mylist) input) (cons (cadar mylist) (get-help (cdr mylist))))
          (else (get-help (cdr mylist)))))
  (get-help (find-to nfa state)))

;;complex procedures
;precondtion: current-state is the list of 'from' atom to test and input could be 'a or 'b.
;postcondition: if input exists for a 'from' in nfa, append and return a list of 'to' states
;(find-to-states nfa ('q0 'q1) 'a) in (('q0 ('a 'q2))('q1 ('a 'q3)))  returns ('q2 'q3).
(define (find-to-states nfa current-state input)
    (cond ((null? current-state) '())
          ;temp-state holds null or list of 'to', given 'a or 'b
          (else (let ((temp-state (get-to-states nfa (car current-state) input)))
                  ;appends a list of temp-state since we cdr current-state
                  (cond ((null? temp-state) (find-to-states nfa (cdr current-state) input))
                        (else (append temp-state (find-to-states nfa (cdr current-state) input)))
                        )))))

;precondition: current state is the 'from' state that will be tested in nfa.
;either a-list or b-list must exist.
;postcondition: returns a new transition in dfa.
;(append-dfa ('q0 'q1)) in  (('q0 ('a 'q2)('b 'q1)('q1 ('a 'q3)))  returns (('q0 'q1) ('a 'q2 'q3)('b 'q1)).
(define (append-dfa current-state a-list b-list)
  ;either of the list must not be null
  (cond ((not (or (null? a-list) (null? b-list)))
         (append current-state (list (flatten(list 'a a-list))) (list (flatten(list 'b b-list)))))
        ((not (null? a-list))
         (append current-state (list (flatten(list 'a a-list)))))
        ((not (null? b-list))
         (append current-state (list (flatten(list 'b b-list)))))
        (else '())))

;;main function
;precondition: a nfa must exist, dfa starts out empty, and stack must be a list of a list which holds
;the inital value of the first 'from' state of nfa.
;postcondition: makes a-list and b-list from the current-state: will first build possible 'to' transitions
;for an 'a and 'b input given a current state.
;makes a new stack by scanning dfa: will put a new state of the 'to' transition into the stack, it shouldn't
;exist in dfa.
;makes a new dfa to append to the current dfa
;terminates if stack is empty, that is, current state is null.
;constructs a complete dfa.
(define (construct-dfa nfa) (display "\nDFA Computations\n")
   (define (iter-dfa dfa stack)
     (let ((current-state (get-test stack)))
       (map display (list dfa "\n"))
       ;returns the dfa once done
       (cond((null? current-state) dfa)
            ;else we append dfa current dfa with the new one
            (else (let* ((a-list (find-to-states nfa current-state 'a))
                         (b-list (find-to-states nfa current-state 'b))
                         (new-stack (update-stack dfa stack a-list b-list))
                         (new-dfa (list(append-dfa current-state a-list b-list))))
                    (if (null? dfa)
                        (iter-dfa new-dfa new-stack)
                        (iter-dfa (append dfa new-dfa) new-stack)))))))
  ;this is the first element in nfa
  (iter-dfa '() (list (list (caar nfa)))))
  
;precondition: dfa is complete and accepting-states should exist in dfa. 
;postcondition: will append a new accepeting sequence will hold the new-accepting states.
(define (new-accept-output dfa accept-states)
  (cond ((null? dfa) '())
        (else (let ((from-states (get-from (car dfa))))
                ;ormap returns the (or (map func list))
                (cond ((ormap (lambda (e) (member? e from-states)) accept-states)
                       (cons from-states (new-accept-output (cdr dfa) accept-states)))
                      ;else we want to keep on cdr down dfa
                      (else (new-accept-output (cdr dfa) accept-states)))))))
                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;test a nfa
(define nfa (list (list 'q0 (list 'a 'q1)(list 'a 'q2))
      (list 'q1 (list 'b 'q3))
      (list 'q3 (list 'a 'q4))
      (list 'q4 (list 'b 'q1))
      (list 'q2 (list 'b 'q5))
      (list 'q5 (list 'a 'q2))))
(define accept-states (list 'q1 'q2))
(display "NFA\n") nfa
(display "\nAccept State:") accept-states

(define dfa (construct-dfa nfa))
(define new-accept-states(new-accept-output dfa accept-states))
(display "\n\nDFA\n") dfa
(display "\nNew Accept States:") new-accept-states
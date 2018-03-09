# Subsets Constructor in Non-determinism - CSC 335
This code is an interpreter that utilizes functional programming to convert a nondeterministic sequence into a deterministic finite automatic sequence. The production of code was based off scholarly researched materials.

## Prerequisites
You might need the following language and software to run the code:
```
Lisp-Scheme
```

## Running the Code
We first need to make up a list of sequence that will be our nfa.  
We then need to set up accept states.
```
(define nfa (list (list 'q0 (list 'a 'q1)(list 'a 'q2))
      (list 'q1 (list 'b 'q3))
      (list 'q3 (list 'a 'q4))
      (list 'q4 (list 'b 'q1))
      (list 'q2 (list 'b 'q5))
      (list 'q5 (list 'a 'q2))))
(define accept-states (list 'q1 'q2))
(display "NFA\n") nfa
(display "\nAccept State:") accept-states
```
Then we run the dfa conversion and produce new accept states (as it might produce more than 1 acceptable state).
```
(define dfa (construct-dfa nfa))
(define new-accept-states(new-accept-output dfa accept-states))
(display "\n\nDFA\n") dfa
(display "\nNew Accept States:") new-accept-states
```

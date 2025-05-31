# Boolean SAT Solver

This project implements a basic Boolean Satisfiability (SAT) Solver in OCaml. It parses logical expressions in Conjunctive Normal Form (CNF) and determines variable assignments that satisfy the given formula.

## Files
- `project2.ml`: Contains the core logic for parsing, evaluating, and solving SAT problems.
- `project2_driver.ml`: Provides utility functions for interacting with the solver.

## Usage
Use the functions in `project2_driver.ml` to parse and solve SAT problems from string inputs.

### Example
Input a CNF formula as a string:

```ocaml
let formula = "((A OR B) AND (NOT A OR C))"
let result = Project2Driver.solve formula
```
Expected Output: \[("A", false); ("B", true); ("C", true)\]

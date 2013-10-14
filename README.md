# simple SAT solver

## overview

This programm tries to find out if a set of prop. logical sentences is satisfiable (that is: is there a assignment of truth-values such that all sentences are true?).

The idea/background for this is me having a look at the first few lessons of the coursere-course [Introduction to Logic](https://www.coursera.org/course/intrologic).
In the second week the basic algorithm I use here is explained/introduced.

The algorithm used to search for solutions tries to find these by backtracking and propagation of known facts
step by step the system of sentences are simplified (by choosing truth-assignments for open variables and propagating the effects)
till either there is a known FALSE fact in the system (in this case backtracking happens and on the top level there is no solution)
or all facts in the system are TRUE in this case the found truth-assignment is returned

## Parser - Description

On top of this sits a simple expression parser that can handle simple strings like this:

 - variables starts with a letter followed by letters or numbers
 - not         = '~'
 - and         = '&'
 - or          = '|'
 - implication = '=>'
 - equivalence = '=='
 - precedence is from top to bottom in this list, sub-sentences can be grouped with parentheses '(..)'

### Example

    p => q | ~r


## Usage
You can solve such a string with a friendly message using satTest or by running main - which ask you for it
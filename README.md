<!-- Land of Scheme - wizardsAdventureGame -->
## Land of Scheme - wizardsAdventureGame

STARTUP: showall

TITLE: Land of Scheme - wizardsAdventureGame

SUBTITLE:  CIS 443 - Programming Languages

date: 2022-03-19

AUTHOR: Yongxiang Cai

<!-- Questions -->
## Questions
* What is the difference betwee ~let~ and ~let*~? Why is the star version used in, for example, the ~look~ function?
- They are different in the order in variables bounds, ~let~ only allow us to define the variables bounds at the same time, variable can't being called in the function and ~let*~ have more flexibility to define the bounds one by one. In the version used in for example the look function use ~let*~ grant us the flexibility which allow us to define the props depends on the world which also depends on cast.

* What is an a-list?
- The a-list is the implementation.

* What does the expression
#+BEGIN_SRC scheme
(a-list-value 'location player)
#+END_SRC
* evaluate to if ~player~ is not a player object (a-list)?
- if ~player~ is not a player object this will return null by the non-declared variable player input which location doesnot exist anywhere. 

* What does the expression
#+BEGIN_SRC scheme
(and player (a-list-value 'location player)))
#+END_SRC
* evaluate to if ~player~ is ~#f~? How does this help with the previous question? Is ~and~ a regular function? How do you know?
- This will return #f. This give us an conditional call ~and~ so that the expect will only print when player are exist(return #t). And is a regular function is all conditional function are considered regular like ~if~ ~or~ etc. Because they all takes the condition to evaluate. In this case print only #t.

* Can you draw a picture of the variable ~a~ after each of the following expressions is evaluated?
#+BEGIN_SRC scheme
(define a '(once fish two fish red fish blue fish))
(set-cdr! (cdr (cdr (cdr a))) '(magenta fish azure fish))
#+END_SRC

* You may want to think about what the first parameter to ~set-cdr!~ evaluates to.
- (once fish two fish magenta fish azure fish)

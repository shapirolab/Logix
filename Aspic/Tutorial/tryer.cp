-language(spifcp).
export(Try).

Try ::= <<
           x, y . First_try + Second_try |
                  Another_try(x) | Another_try(y) .

           First_try ::= x ? {a} , a ! [] , 0 .

           Second_try ::= y ? {a} , a ! [] , 0 
        >> .

Another_try(x) + b ::= x ! {b} , b ? [] , 0 .

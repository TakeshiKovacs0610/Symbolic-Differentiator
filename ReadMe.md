# Symbolic differentiator

## About

This project uses ocamllex and ocamlyacc to convert the input function to a parse tree on which it applies the differentiation algorithm. After a round of simplification, it converts the resulting parse tree back into the output function.

## How to run this project ? 

1. Clone this repository to your local machine : 

    ```
    gh repo clone PoojanCShah/Symbolic-Differentiator
    cd Symbolic-Differentiator
    ```

2. Initialize the Lexer and Parser : 
    ```
    make init
    ```

3. Run and enter your function (variable x) in the terminal. Then press enter to get the answer
    ```
    make run
    ```
4. To clean up the directory : 
    ```
    make clean
    ```

## Features

1. Elementary exponential, logarithmic, trignometric functions and their compositions along with basic arithmetic operations.

2. The function is assumed to be of the variable `x` and other variables are taken as symbols. The symbols can start with lowercase letters and thereafter can contain alphanumerics and underscores

3. Please use the following conventions : 
    
    1. Addition : `+`
    2. Subtraction : `-`
    3. Multiplication : `*`
    4. Division : `/`
    5. Power  : `^`
    6. Trig functions : `sin(x), cos(x), tan(x), sec(x), csc(x), cot(x)`
    7. Log and Exp : `log(x), e^(x), 3^x`
## Examples

```
tan(alfa*e^(k*x))
((sec((alfa * e^((k * x)))) ^ 2) * (alfa * (e^((k * x)) * k)))
```

```
log(1+sin(x^2))
(((2 * x) * cos((x ^ 2))) / (1 + sin((x ^ 2))))
```

```
1+2*x+3^x 
(2 + ((3 ^ x) * log(3)))
```
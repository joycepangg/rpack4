# rpack4
These are the functions for rapck4. This package includes the cheat function and the remind_me function. 

## Installation
Install through 

    install_github("joycepangg/rpack4")

***Note** make sure to download "devtools" before installation* :smiley:

## Usage
There are :two: main functions in this package including the cheat function and the remind_me function. 

cheat() allows you to get the answer to question 1,2,6,16 from week 3 Assignment from the Course "Programming in Psychological Science" at Uva
    
    Example 
    
    cheat(16) returns 
                      [,1] [,2] [,3]
                [1,]    1    2    3
                [2,]    8   10   12
                [3,]   21   24   27
What if you type in a number that is not 1,2,6 or 16? No worries! The function will remind you to type a valid number. 

    Example 
    
    cheat(9) returns 
    
                 "please type a number that is either 1,2,6 or 16"

remind_me() reminds you of the birthday of the package author!
 
 remind_me() returns 
                     

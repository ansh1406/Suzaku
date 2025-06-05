# Initiation of Program

> Push the return address

> Jump to the first declared function

> Add HALT instruction at return address

# Function Declaration

> Create a label with function id provided by IR generator

# Global Variable Declaration

> Get the first empty memory cell address

> Load the initial value (if initialized)

> Mark the chosen memory cell as occupied

# Global Variable Access

> Address will already be harcoded

> Load valur stored at address to register A

> Push value to stack

# Global Constant Declaration

> Compilation time entity (will directly be replaced with it's value at compile time)

# Global Constant Access

> Compilation time entity (will directly be replaced with it's value at compile time)

# Local Constant Declaration

> Compilation time entity (will directly be replaced with it's value at compile time)

# Local Constant Access

> Compilation time entity (will directly be replaced with it's value at compile time)

# Local Variable Declaration

> Solve expression for initial value/s (if applicable)

> Leave empty space for uninitialized values

> Push value/s onto the stack from last occurance to first occurance manner

# Local Variable Access

> Load the base stack pointer to reg A

> Add offset (calulated at compile time) to the base stack pointer

> Push value to the stack

> Calculate offeset because of dimentions (if applicable)

> Push value to the stack

> Fetch both values from stack and add them

> Load value from calculated address to reg A

> Push the value to the stack

# Binary Operation

> Push left value to the stack

> Push right value to the stack

> Load right value to reg B

> Load left value to reg A

> Perform the operation

> Push The Result to the stack

# Unary Operation

> Push operand to stack

> Load operand to reg A

> Perform the operation

> Push the Result to the stack

# Function Call

> Leave an empty cell on stack for return value

> Push the parameters on to the stack

> Push the return address to stack

> Jump to the function

# Branching

> Create lables for true block, false block and end of the branching

> Check condition

> Jump to appropriate block

> Jump to branch ending after the last statement of true block

# Switch

> Create labels for all cases and default case (even if it was not provided) based on a pre defined naming convention

> Create a label for the end of switch statement

> All break statements will jump to the end of switch statement

> Solve expression inside switch and push the value to the stack

> Match the value with all given cases and jump to appropriate label

> Jump to default label if value remained unmatched

# For Loop

> Run the initialization expression

> Create labels for condition check , starting of body , updation expression , end of loop

> Uncoditional jump statement to the condition check will be added after the updation expression

> Check condition and jump to start of body in case of true and end of loop in case of false

> Break statement will make the program jump to end of loop

> Continue statement will make the program jump to the updation expression

# While Loop

> Create labels for condition check, start of body , end of body

> Check condition and jump to appropriate label

> Break statement will make the program jump to end of loop

> Continue statement will make the program jump to the condition check

# Do-While Loop

> Create labels for starting of loop , condition check , ending of loop

> Program will keep running without any disturbance

> Check condition and jump to appropriate label

> Break statement will make the program jump to end of loop

> Continue statement will make the program jump to the condition check

# Case 

> Will create a lable based on case id and switch id of switch in which the case was

# Default Case

> Will create a lable based on switch id of switch in which it was

# Return

> Solve the return expression (0 will be default value if nothing was provided)

> Calculate address of cell provided for return value

> Store the value there

> Calculate address of cell in which return address was provided

> Store the return address in register A

> Jump

# Multi-Instructional Binary Operations (Operation not possible in one instruction)

## Relational Operation

> Less than : Jump to set true label if value was lesser

> Less than or euqals : Jump to set false label if value was greater

> Greater than : Jump to set true label if value was greater

> Greater than or euqals : Jump to set false label if value was lesser

> Equals : Jump to set true label if value was equal

> Not Equals : Jump to set false label if value was equal

## Logical Operation

> Logical AND : Jump to set false label if any one of the values was 0

> Logical OR : Do Bitwise OR and if result was 0 jump to set false label

## Bitwise shift Operation

> Left shift : Make a loop and keep multiplying left operand by 2 as many times as value of right operand

> Right shift : Make a loop and keep dividing left operand by 2 as many times as value of right operand

# Multi-Instructional Unary Operations (Operation not possible in one instruction)

> Logical NOT : Jump to set false block if value is equal to 0

> AddressOf : 

> > Load the base address of variable

> > Add dimensional offset to it

> > Push the value to stack

> Dereference : Solve the expression and push the value at that address to stack

# Built-in Functions

## For Printing Integer (__printInt(value))

> Store the value at cell provided for return value

> Print '-' if the value is negative

> Push a termination character to stack (-1 in my case)

> Load value to register A

> Do modulus by 10 and push the result to stack

> Divide value by 10 

> Check if value is equal to 0

> Jump to atcual printing part if value is 0

> Or load new value to register A and keep repeating

> Actual printing part :

> > Load value from the stack

> > Compare it with the terminator 

> > Jump to return address if equal

> > Add 48 (ASCII of '0') to the value

> > Print it

## For Printing A Single Character (__printChar(c))

> Store the character to cell provided for return value

> Print the character

> Jump to the return address

## For Printing String (__printString(address of first character of string))

> Store 0 at cell provided for return value

> Load value from address

> Print the value and add 1 to the address

> Keep repeating until null character is reached

> Jump to the return address

## For Reading Integer from the Keyboard (__readInt(address))

> Push -1 to the stack if '-' was the first input

> Else push 1 to the stack

> Push 0 to the stack

> Actively log for input

> Jump to the second part if newline was inputted

> Subtract 48 (ASCII of '0') from the inputted character

> Load value from stack multiply it by 10 and the result into it

> Push the result to the stack

> Keep repeating

> Second Part : 

> > Load value from stack to register A
> > Load value from stack to register B
> > Multiply them
> > Store the result at provided address and cell provided for return value
> > Jump to the return address

## For Reading Character from the Keyboard (__readChar(address))

> Read one character from the keyboard

> Store it the provided address and the cell provided for return value

> Jump to the return address

## For Reading String from the Keyboard (__readString(address where first character will be stored))

> Load address to register D

> Read one character from the keyboard

> Compare it with newline character and if equal store 0 at cell provided for return value and jump to return address

> Store it to the address at register D

> Copy address from register D to register A

> Add 1 to it and again store it to register D

> Keep repeating
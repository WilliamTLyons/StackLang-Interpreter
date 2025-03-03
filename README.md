# StackLang Interpreter

StackLang is a robust and extensible stack-based programming language interpreter implemented in OCaml. It provides an environment for executing arithmetic operations, boolean logic, variable management, and control flow using a stack-based evaluation model. Built with a parser combinator framework and a comprehensive evaluator, StackLang serves as both an educational tool for understanding language implementation and a practical utility for experimenting with stack-based computation.

## Project Goals

The StackLang interpreter was developed with the following objectives:
- **Educational Insight**: Demonstrate the implementation of a programming language using parser combinators and a stack-based evaluator in OCaml.
- **Flexibility**: Provide a modular framework that supports a diverse set of operations, from basic arithmetic to scoped variables and control flow.
- **Reliability**: Ensure robust error handling for invalid inputs or operations, making it suitable for experimentation and learning.

## Features

- **Stack Operations**: Efficiently push constants, pop values, and trace stack contents.
- **Arithmetic**: Perform addition, subtraction, multiplication, and division over a configurable number of stack elements.
- **Boolean Logic**: Execute AND, OR, and NOT operations on boolean values.
- **Comparisons**: Compare integers with equality and less-than-or-equal checks.
- **Variables**: Manage local and global variable bindings with lookup functionality.
- **Control Flow**: Implement conditional execution and nested blocks with if-else and Begin/End constructs.

### Stack Data Structure
![image](https://github.com/user-attachments/assets/4bee6949-53f4-4263-b4d8-f42b53d0d62b)

## Usage

StackLang operates by interpreting a string of commands, where each command is separated by a newline. The interpreter outputs a list of traced values (as strings) in reverse chronological order, reflecting the most recent `Trace` outputs first. Use the `interp` function in an OCaml environment to execute programs.

### Supported Commands

- **`Push <const>`**: Pushes a constant (integer, `True`, `False`, `()`, or name) onto the stack.
- **`Pop <n>`**: Removes the top `n` items from the stack.
- **`Trace <n>`**: Logs the top `n` stack items (output in reverse order).
- **`Add <n>`**: Adds the top `n` integers.
- **`Sub <n>`**: Subtracts the sum of the next `n-1` integers from the top integer.
- **`Mul <n>`**: Multiplies the top `n` integers.
- **`Div <n>`**: Divides the top integer by the product of the next `n-1` integers.
- **`And`**: Logical AND of the top two booleans.
- **`Or`**: Logical OR of the top two booleans.
- **`Not`**: Logical NOT of the top boolean.
- **`Equal`**: Checks if the top two integers are equal.
- **`Lte`**: Checks if the top integer is less than or equal to the next integer.
- **`Local`**: Binds a name (top) to a value (next) in the local scope.
- **`Global`**: Binds a name (top) to a value (next) in the global scope.
- **`Lookup`**: Retrieves the value of a name from local or global scope.
- **`Begin ... End`**: Executes a block, pushing its final value to the outer stack.
- **`If ... Else ... End`**: Conditional execution based on the top boolean.


## Notes

- **Output Order**: Traced values are returned in reverse chronological order (most recent first).
- **Error Handling**: Invalid operations (e.g., type mismatches, stack underflow, division by zero) result in an output of `["Error"]`.

## Examples

Below are sample programs demonstrating StackLang's core functionality. Outputs are lists of traced values in reverse chronological order (most recent first).

1. **Basic Stack Operations**
<img width="778" alt="Screenshot 2025-03-03 at 7 51 19 AM" src="https://github.com/user-attachments/assets/9488209f-e955-443e-8b79-c3580cff7e07" />

Pushes an integer onto the stack and traces it.
** **

2. **Arithmetic (Addition and Subtraction)**
<img width="778" alt="Screenshot 2025-03-03 at 7 53 33 AM" src="https://github.com/user-attachments/assets/ca29354c-464c-4e84-b8d3-ec2b674dd101" />

Adds two numbers (10 + 20 = 30), then subtracts (20 - 100 = -80 due to stack order). Note the reverse order: latest trace (-80) first.
** **

3. **Boolean Logic**
<img width="778" alt="Screenshot 2025-03-03 at 7 58 38 AM" src="https://github.com/user-attachments/assets/d4be1fa9-8296-4087-b6b3-6d22f9dc0fdf" />

Performs a logical OR operation on two booleans.
** **

4. **Comparison**
<img width="778" alt="Screenshot 2025-03-03 at 8 00 55 AM" src="https://github.com/user-attachments/assets/0f1d6651-aa2a-4f3f-a867-abb7bd88932b" />

Checks if the top integer (5) is less than or equal to the next integer (3). Returns `False` since 5 > 3.
** **

5. **Variables and Lookup**
<img width="778" alt="Screenshot 2025-03-03 at 8 49 50 AM" src="https://github.com/user-attachments/assets/3622e7d3-b6d3-4dca-9736-e7aa2b5c744d" />

Binds the name `x` to 10 locally and retrieves its value.
** **

6. **Control Flow with Blocks**
<img width="778" alt="Screenshot 2025-03-03 at 8 51 18 AM" src="https://github.com/user-attachments/assets/4a2b786e-713f-49ae-b491-29b92d5883b2" />

Executes the else branch based on the top boolean, tracing 23.
** **

## Technical Details

- **Language**: Written in OCaml, utilizing its strong typing and pattern-matching features.
- **Parsing**: Employs parser combinators for flexible and composable command parsing, built upon a character list-based input model.
- **Evaluation**: Uses a recursive evaluator that maintains a stack, local/global variable environments, and a log of traced values.
- **Error Handling**: Implements comprehensive checks for stack underflow, type mismatches, and invalid operations, returning `["Error"]` in such cases.
- **Output**: Traced values are collected in a list, prepended in reverse order (latest first), reflecting the stack’s state at each `Trace` command.

## Known Limitations

- **Performance**: Stack operations use list concatenation (`@`), which may be inefficient for very large programs.
- **Scope Persistence**: Local variables are scoped to their block but do not persist across separate `interp` or `interp_file` calls.

## Acknowledgments

- **Professor Abbas Attarwala**: Provided the starter code for utility functions and parser combinators, forming the foundation of this project.
- **William Lyons**: Designed and implemented the command language, evaluator, and additional parsing logic.

## Additional Resources

- [OCaml Official Website](https://ocaml.org/) – Learn more about OCaml.
- [Try OCaml Pro](https://try.ocamlpro.com/) – Test the code yourself online.

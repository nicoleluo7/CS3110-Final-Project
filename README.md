# CS3110-Final-Project

## Project Description
How to run our project?
First you need to download ZIP file named CS3110-Final-Project-main from our GitHub repo, unzip it and cd to its directory. To build and run our system, first enter command: `dune build`, then input: `dune exec ./src/main.exe`. You will then see our Propositional Logic REPL v1.0 with commands you could choose to input.

**Valid commands include:**
- `premise <formula>` - Add a premise (automatically applies basic inference rules)
- `goal <formula>` - Set the goal (automatically applies basic inference rules)
- `applyall` - Apply ALL inference rules exhaustively
- `show` - Display current proof state
- `reset` - Clear entire proof state
- `find <formula>` - Find possible ways to derive a formula
- `help` - Show help message
- `shortcuts` - Print command shortcuts
- `rules` - List all supported inference rules
- `quit` - Exit the REPL

The `<formula>` supports propositional logic formulas with:
- Variables: `A`, `B`, `C`, etc.
- Conjunction: `A & B`
- Disjunction: `A | B`
- Implication: `A -> B`
- Negation: `!A`

If the input is empty or syntactically invalid (e.g. `A &`, `!`, `A ->`, `A && B`, `A => B`, `A B`, invalid variable), it raises a `Parse_error`.

# Authors
Nicole Luo (nl545)
Tyson Yoshikawa (tmy25)
Shirley Wu (ww537)
Anna Sahakyan (as4275)

## Collaborators
- We didn't collaborate with anyone outside our team on the project.

## GenAI Usage
We used generative AI tools for the following purposes:
- **Code documentation**: Generating comments and function specifications
- **Debugging assistance**: Identifying and fixing bugs
- **Code refactoring**: Suggestions for improving code structure and removing redundancy
- **Syntax help**: Assistance with OCaml syntax and best practices
- **Code analysis**: Understanding codebase structure and explaining functionality

All code logic, design decisions, and implementation were done by the team members. GenAI was used as a tool to assist with development, not to generate the core implementation. 
# CS3110-Final-Project

## Project Description
How to run our project?
First you need to download ZIP file named CS3110-Final-Project-main from our GitHub repo, unzip it and cd to its directory. To build and run our system, first enter command: dune build, then input: dune exec ./src/main.exe. You will then see our Propositional Logic REPL v1.0 with commands you could choose to input. Valid commands include premise <formula> to add a premise; goal <formula> to set the goal; derive to apply Modus Ponens rule. The <formula> supports four forms of input now: A; A & B; A -> B; !A. If the input is empty or syntactically invalid (e.g. A &, !, A ->, A && B, A => B, A B, invalid variable), it raises a Parse_error. You can also input show, reset, help, shortcuts and quit based on your needs.

# Authors
Nicole Luo (nl545)
Tyson Yoshikawa (tmy25)
Shirley Wu (ww537)
Anna Sahakyan (as4275)

#Collaborators:
- We didn't collaborate with anyone outside our team on the project.
- GenAI Usage: We used ChatGPT to generate some comments and specifications of the functions, sometimes debugging, some syntax and stucture suggestions. 
# Internal workflows for Charm

## Introduction

This document exists to record some of the workflows that go on in Charm when it does things. Unless you are interested in helping me to develop Charm (in which case thank you very much) there is no need for you to know any of this.

## Initialization workflow

To start a script, the user types `hub run <program name>` into the REPL. The command `run <program name>` is passed to the Hub, the housekeeper of scripts and services, which passes the name of the program to the Initializer. The Initializer gets the text of the program and passes it to the Relexer, which passes it to the Lexer.

The Lexer then passes tokens one at a time to the Relexer, which is a sort of kludge to take care of the syntactic whitespace and generally to make the syntax more suitable for the Parser and less suitable for human beings.

The Relexer passes the tokens one at a time to the Initializer, which lightly processes the tokens still further and sorts them out into Tokenized Code Chunks, each being a separate variable initialization / function/command definition / type definition. It sorts the TCCs according to their purpose, constant definitions in one array, variables in another, type definitions, public functions, private functions, etc.

The Initializer then creates a parser and starts reading the words from the function signatures into it so that the Parser knows what's a prefix, infix, suffix, or whatever. We then parse and evaluate the type definitions, so we know what the types are. Then we create the Functions and attach each one to a FunctionTree that allows us to resolve the multiple dispatch at runtime. We create Environments to hold the global constants and variables. We put the TCCs containing constant/variable initializations in the right order (checking for cyclic definitions) and the Evaluator evaluates them in the context of the appropriate Environments, thus initializing them.

We end up with a Parser and an Environment. The Initializer returns those to the hub as a Parser-Environment pair: a Service.

The Hub sets its current Service to be the one you just initialized. The next thing you type into the REPL will be parsed by the Service's Parser and then evaluated by the Evaluator in the context of the Service's Environment and of the FunctionTree in the Parser. In short, your script is now running!
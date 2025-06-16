
# English Formal Semantics Interpreter

This is a CLI for parsing and evaluating English sentences using the principles of formal semantics.

Tokenization is performed by integration with SpaCy via the pyml module.

## Features

- Parses basic English syntax (e.g. *"The cat sleeps."*)
- Converts sentences into logical forms using lambda calculus
- Evaluates truth in a model-theoretic semantics framework
- Interactive REPL and file mode
- Integration with spaCy for robust tokenization

## Installation

### 1. Install OCaml and Dune
```bash
opam install dune
```

### 2. Install Python and spaCy

```
pip install spacy
python -m spacy download en_core_web_sm
```

### 3. Clone and Build the Project
```
git clone https://github.com/YOUR_USERNAME/formal-semantics
cd formal-semantics
dune build
```

## Usage 

### Start the REPL
```
dune exec bin/main.exe
```

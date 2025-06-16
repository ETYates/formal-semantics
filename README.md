
# English Formal Semantics Interpreter

This is a CLI for parsing and evaluating English sentences using the principles of formal semantics.

Tokenization is performed by integration with SpaCy via the pyml module.

## Features

- Parses basic English syntax (e.g. *"The cat sleeps."*)
- Converts sentences into logical forms using lambda calculus
- Evaluates truth in a model-theoretic semantics framework
- Interactive REPL and file mode
- Integration with spaCy for robust tokenization

## Example

Sentences are interpreted as predicates interpreted against a model. 

A sentence ending with a period is a **declaration** that will modify the model
so that the sentence is true when interpreted against the model. 

A sentence ending with a question mark is a **query** that will return the truth
value or denotation of a sentence without modifying the model.

The interpreter can reason with syllogisms:

```
> Socrates is a man.
> Every man is mortal.
> Is Socrates mortal?
Yes
> Who is mortal?
Socrates
```

Additionally, it is possible to reference entities with definite description.

```
> Aristotle is a philosopher.
> The philosopher is Greek.
> Who is Greek?
Aristotle
```

Wh-questions will return whichever answers have already been declared.

```
> Aristotle is a philosopher.
> The philosopher is Greek.
> Who is Greek?
Aristotle
```

There are three available flags.

In order to print the logical form, use the flag `--show-lf`

```
> Aristotle is a man. --show-lf
man(aristotle) : t
> Every man is mortal. --show-lf
@x[man(x) -> mortal(x)] : t
```

Use `--show-tree` to show the syntax tree and logical evaluation.

```
> Aristotle is a philosopher. --show-tree
t [ philosopher(aristotle) : t
  [ Aristotle : d  :  ]
  t [ philosopher(aristotle) : t
    [ is : t  :  ]
    v [ philosopher(aristotle) : t
      [ d |- aristotle : e ]
      v [ \x.philosopher(x) : e -> t
        [ v |-  :  ]
        d [ \x.philosopher(x) : e -> t
          [ a : d  :  ]
          [ philosopher : n \x.philosopher(x) : e -> t ]
        ]
      ]
    ]
  ]
]
```

The flag `--show-model` will show the model with all prior declarations.

```
> Socrates is a philosopher.
> Plato is a philosopher.
> Aristotle is a philosopher.
> Socrates taught Plato.
> Plato taught Aristotle. --show-model
<Aristotle, Plato and Socrates>
philosopher : <Aristotle, true>,<Plato, true>,<Socrates, true>
teach : <Plato, <Aristotle, true>>,<Socrates, <Plato, true>>
{teach(plato,aristotle), teach(socrates,plato), philosopher(aristotle), philosopher(plato), philosopher(socrates)}
```

There are also two commands. 

The model can be shown without typing a sentence by entering `Show model!`.

Type `Quit!` to exit the program.

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

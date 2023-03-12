# Regex-Lexing-in-Scala

An implementation of a Regex Engine and Lexer done in Scala as part of a Formal Languages and Automata course from late 2022.

The point of the assignment was to build an understanding about finite state machines and formal language theory.

# Regex

We start from the regex itself in string form, the syntax is similar to usual regex implementations found in standard libraries, but less syntactic "sugars" :
- character literal
- [character]"|"[character] => UNION of two characters, can be interpreted as "OR"
- [character][character] => CONCATENATION of two character, can be interpreted as "AND"
- [character]* => KLEENE STAR, can be interpreted as "0 or more times"
- [character]? => MAYBE, can be interpreted as "0 or exactly once"
- [character]+ => PLUS, can be interpreted as "once or more times"
- [A-Z], [a-z], [0-9] => Syntactic sugar for all characters from capital A to Z, lowercase a to z and digits 0 through 9.
- Escaping is done by placing the character in '' 's

We transform this Regex using Djikstra's Shunting Yard algorithm into a Prefix notation we call "Prenex", at this step we also
handle the syntactic sugars [A-Z] and the others by transforming them into a series of literals unified with each other.

We then build an Abstract Syntactic Tree using the Prenex notation in order to ease the construction of an NFA in the following step.

We use Thompson's Construction to construct an NFA (Nondeterministic Finite Automaton) from the Prenex we generated at the last step, followed by Subset Construction to convert the NFA to a DFA (Deterministic Finite Automaton).

The Lexer itself takes in a configuration file with the specification listed as follows:
- #TOKEN_NAME: [REGEX];

The implementation creates a DFA that concatenates all the token regexes and remembers their final states, it then runs the automaton on the input and returns the longest matching token it finds, or it specifies at which line and what character a potential error has occured if it ever reaches its sink state.

# Improvements
This was my first project done in Scala so I missed out on a lot of its features early in development, a few things are done in somewhat hacky and ugly ways. I would like to return to this project and make the code cleaner in the future.

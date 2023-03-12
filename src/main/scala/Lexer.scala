import scala.collection.immutable.ListMap

case class Lexer (spec: String) {

  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */

  val (dfa, dfaLexMap) = init()

  // Parse spec
  private def init():(Dfa[Int], ListMap[String, Set[Int]]) = {
    val vec: Vector[(String, String)] = spec.split(";\n").foldRight(Vector[(String, String)]())((x, acc) => {
      val split: Array[String] = x.split(": ")
      acc.+:((split(0), split(1)))
    })

    var nfa: Nfa[Int] = Nfa.empty()
    var nfaLexMap: ListMap[String, Set[Int]] = ListMap()
    for ((lex, regex) <- vec) {
      // Create an NFA for the given regex
      val prenex = Regex.toPrenex(regex)
      val regexNfa = Nfa.fromPrenex(prenex)
      nfaLexMap = nfaLexMap + (lex -> regexNfa.map(_ + nfa.stateCount).finalState)
      nfa = nfa.append(regexNfa)(x => x + nfa.stateCount)
    }

    // Minimise it or not, for now.
    //Dfa.fromLexNfa(nfa, lexStateMap)
    val (dfa, tempLex) = Dfa.fromLexNfa(nfa, nfaLexMap)

    // Order the LexMap so that it follows the order of the specification
    val lexemes : Vector[String] = vec.foldRight(Vector[String]())((x, acc) => acc.+:(x._1))
    val orderedLex : ListMap[String, Set[Int]] = ListMap.empty[String, Set[Int]] ++ lexemes.map(key => key -> tempLex(key))

    (dfa, orderedLex)
    //Dfa.minimiseLex(tempdfa, templex)
  }

  def lex(word: String): Either[String,List[(String,String)]] = {
    def findLexem(state: Int) : String = {
      for ( (lexem, states) <- dfaLexMap ) {
        if (states.contains(state)) {
          return lexem
        }
      }
      "Not Found"
    }

    // Parse spec
    var right : List[(String, String)] = List()
    var progress = word
    var charProg = 0
    var lineProg = 0
    do {
      val (read, remaining, state, prog, line) = dfa.read(progress, charProg, lineProg)

      val lex = findLexem(state)
      if (lex == "Not Found") {
        val tmp = if (prog == progress.length) "EOF" else prog.toString
        return Left("No viable alternative at character " + tmp + ", line " + line)
      } else {
        right = right.:+(read -> lex)
      }

      progress = remaining
      charProg = prog
      lineProg = line
    } while(progress.nonEmpty)
    Right(right)
  }
}
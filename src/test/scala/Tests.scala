import scala.collection.mutable

class Tests extends munit.FunSuite {

  test("Reverse") {
    //println(Nfa.fromPrenex("eps"))
    //println(Nfa.fromPrenex("eps").accepts(""))

    val dfa = Dfa.fromPrenex("CONCAT a CONCAT UNION UNION a STAR UNION a b PLUS b d")
    val revDfa = dfa.reverse()
    val minDfa = Dfa.minimise(dfa)
    println(dfa.toString)

    println(revDfa)

    println(minDfa.toString)

    val str = "aaaaaaaaaccccc"
    assert(dfa.accepts(str) == minDfa.accepts(str))

    val s = "bbbbccccccccc"
    assert(dfa.accepts(s) == minDfa.accepts(s))
  }

  test("Personal Test"){
    val spec =
      """A: a;
        #BC: bc;
        #DEF: def;
        #""".stripMargin('#')


    assert(Lexer(spec).lex("aa") == Right(List(("a", "A"), ("a", "A"))))
    assert(Lexer(spec).lex("abca") == Right(List(("a", "A"), ("bc", "BC"), ("a", "A"))))
    assert(Lexer(spec).lex("abcdefdefbca") == Right(List(("a", "A"), ("bc", "BC"), ("def", "DEF"), ("def", "DEF"), ("bc", "BC"), ("a", "A"))))
  }
}
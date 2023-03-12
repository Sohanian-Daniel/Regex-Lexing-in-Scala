import scala.collection.mutable

sealed trait AST {
  // Try to cool queue
  def hasEmpty: Boolean = {
    this match {
      case UNION(x, y) => x.hasEmpty || y.hasEmpty
      case CONCAT(x, y) => x.hasEmpty || y.hasEmpty
      case STAR(x) => x.hasEmpty
      case PLUS(x) => x.hasEmpty
      case MAYBE(x) => x.hasEmpty
      case ATOM(_) => false
      case EPSILON => false
      case EMPTY => true
    }
  }

  def insertAtEmpty(value: AST): AST = {
    this match {
      case UNION(x, y) => if(x.hasEmpty) UNION(x.insertAtEmpty(value), y) else UNION(x, y.insertAtEmpty(value))
      case CONCAT(x, y) => if(x.hasEmpty) CONCAT(x.insertAtEmpty(value), y) else CONCAT(x, y.insertAtEmpty(value))
      case STAR(x) => STAR(x.insertAtEmpty(value))
      case PLUS(x) => PLUS(x.insertAtEmpty(value))
      case MAYBE(x) => MAYBE(x.insertAtEmpty(value))
      case EMPTY => value
      case _ => this
    }
  }

  def unravel(): AST = {
    this match {
      case UNION(x, y) => UNION(x.unravel(), y.unravel())
      case CONCAT(x, y) => CONCAT(x.unravel(), y.unravel())
      case STAR(x) => STAR(x.unravel())
      case PLUS(x) => CONCAT(x.unravel(), STAR(x.unravel()))
      case MAYBE(x) => UNION(x.unravel(), EPSILON)
      case EMPTY => EMPTY
      case EPSILON => EPSILON
      case ATOM(a) => ATOM(a)
    }
  }

}

object AST {
  def apply(str: String): AST = {
    val stack = mutable.Stack[AST]()
    val split = str.split(" ")
    var list : List[String] = List()

    var i : Int = 0
    while (i < split.length - 1) {
      if (split(i) == "'" && split(i + 1) == "'") {
        list = split(i+1) :: list
        i = i + 2
      } else if (split(i) == "'") {
        list = "''" :: list
        i = i + 1
      } else {
        list = split(i) :: list
        i = i + 1
      }
    }
    if (split.length == 1) {
      if (split(i) == "'") {
        list = "''" :: list
      } else {
        list = split(i) :: list
      }
    } else if (i < split.length) list = split(i) :: list
    list = list.reverse

    for (token <- list) {
      // Match from string return AST datatype
      val value: AST = token match {
        case "UNION" => UNION(EMPTY, EMPTY);
        case "CONCAT" => CONCAT(EMPTY, EMPTY);
        case "STAR" => STAR(EMPTY);
        case "PLUS" => PLUS(EMPTY)
        case "MAYBE" => MAYBE(EMPTY)
        case "eps" => EPSILON
        case "void" => EMPTY
        case "\'" => ATOM(' ')
        case "''" => ATOM('\'')
        case c => if (c.charAt(0) == '\'') {ATOM(c.charAt(1))} else {ATOM(c.charAt(0))}
      }
      if (stack.nonEmpty) {
        if (stack.top.hasEmpty) {
          val top = stack.pop()
          stack.push(top.insertAtEmpty(value))
        } else {
          return EMPTY
        }
      } else {
        // Add to queue
        stack.push(value)
      }
    }

    val result = stack.pop()
    result
  }
}

final case class ATOM(a: Char) extends AST
final case class UNION(first: AST, second: AST) extends AST
final case class STAR(atom: AST) extends AST
final case class CONCAT(first: AST, second: AST) extends AST
final case class PLUS(atom: AST) extends AST
final case class MAYBE(atom: AST) extends AST
case object EPSILON extends AST
case object EMPTY extends AST


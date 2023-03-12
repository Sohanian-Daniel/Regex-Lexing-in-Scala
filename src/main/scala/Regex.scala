import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Regex {
  def isOperator(c: String): Boolean = {
    c match {
      case "|" => true
      case "*" => true
      case "+" => true
      case "?" => true
      case "." => true
      case "(" => true
      case ")" => true
      case _ => false
    }
  }

  def operatorPrecedence(c: String): Int = {
    c match {
      case "|" => 1
      case "*" => 3
      case "+" => 3
      case "?" => 3
      case "." => 2
      case _ => 0
    }
  }

  def isControl(c: String):Boolean = {
    (c == "(") || c(0).isLetterOrDigit
  }

  def findConcat(s: String): List[String] = {
    var tokens: List[String] = List()

    var i : Int = 0
    val preProcessing: ArrayBuffer[String] = ArrayBuffer()

    while (i < s.length) {
      s(i) match {
        case '\'' => preProcessing.+=("'" + s(i+1) + "'"); i = i + 2//.append(s(i+1)); i = i + 2
        case '[' =>
          s(i+1) match {
            case '0' => "(0|1|2|3|4|5|6|7|8|9)".foreach(c => preProcessing.+=(c.toString))
            case '1' => "(1|2|3|4|5|6|7|8|9)".foreach(c => preProcessing.+=(c.toString))
            case 'a' => "(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z)".foreach(c => preProcessing.+=(c.toString))
            case 'A' => "(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z)".toUpperCase.foreach(c => preProcessing.+=(c.toString))
          }
          i = i + 4
        case _ => preProcessing.+=(s(i).toString)
      }

      i = i + 1
    }
    def addConcatSymbolIfNotOperator(str: String): Unit = {
      tokens = "." :: (if (!isOperator(str)) "'" + str + "'" else str) :: tokens
    }

    i = 0
    while(i < preProcessing.length - 1) {
      val str = preProcessing(i)
      val next = preProcessing(i + 1)
      if (!isOperator(str) && next == "(") {
        addConcatSymbolIfNotOperator(str)
      } else if (!isOperator(str) && !isOperator(next)) {
        addConcatSymbolIfNotOperator(str)
      } else if (str == ")" && !isOperator(next)) {
        addConcatSymbolIfNotOperator(str)
      } else if (str == ")" && next == "(") {
        addConcatSymbolIfNotOperator(str)
      } else if ((str == "*" || str == "?" || str == "+") && isControl(next)) {
        addConcatSymbolIfNotOperator(str)
      } else {
        if (!isOperator(str)) tokens = ("'" + str + "'") :: tokens
        else tokens = str :: tokens
      }
      i = i + 1
    }
    if (preProcessing.last != "\'" && !isOperator(preProcessing.last)) tokens = "'" + preProcessing.last + "'" :: tokens
    else if (preProcessing.last != "\'") tokens = preProcessing.last :: tokens
    if (preProcessing.last == " ") tokens = "'" + preProcessing.last + "'" :: tokens
    tokens.map(_.replace("''", "'")).reverse
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    if (str == "eps") return str
    if (str == "' '") return str
    val replaced = str.replaceAll("\\\\n", "\n").replaceAll("\\\\t", "\t")

    val operation_stack = mutable.Stack[String]()
    val output_queue = mutable.Queue[String]()

    val tokens: List[String] = findConcat(replaced)
    for (token <- tokens) {
      token match {

        case "(" => operation_stack.push(token)
        case ")" =>
          while(operation_stack.top != "(") { output_queue.enqueue(operation_stack.pop())}
          operation_stack.pop()
        case x =>
          if (!isOperator(x(0).toString)) output_queue.enqueue(token)
          else {
              def iterPrecedence(): Unit = {
                while (operatorPrecedence(x(0).toString) < operatorPrecedence(operation_stack.top(0).toString)) {
                  output_queue.enqueue(operation_stack.pop())
                  if(operation_stack.isEmpty) return
                }
              }
            if(operation_stack.nonEmpty) iterPrecedence()

            operation_stack.push(x)
          }
      }
    }
    while(operation_stack.nonEmpty) {
      output_queue.enqueue(operation_stack.pop())
    }

    val stack : mutable.Stack[String] = mutable.Stack()

    for (token <- output_queue.toList) {
      token match {
        case "|" => val first = stack.pop()
          val second = stack.pop(); stack.push("UNION " + second + " " + first)
        case "." => val first = stack.pop()
          val second = stack.pop(); stack.push("CONCAT " + second + " " + first)
        case "*" => stack.push("STAR " + stack.pop())
        case "?" => stack.push("MAYBE " + stack.pop())
        case "+" => stack.push("PLUS " + stack.pop())
        case _ => stack.push(token)
      }
    }
    val result = stack.pop()
    result
  }
}

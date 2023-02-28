import scala.collection.mutable

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  val concatChar = '\u00B7'
  val op: List[Char] = List('|', concatChar)
  val leftOp: List[Char] = List('*', '+', '?')
  var precedence: Map[Char, Int] = Map[Char, Int]()
  var opToString: Map[Char, String] = Map[Char, String]()

  def syntaxSugar(str: Char, str1: Char): List[Char] = {
    var ret = List[Char]()
    for (i <- str until str1) {
      ret :+= i
      ret :+= '|'
    }

    ret :+= str1
    ret
  }

  def preprocess(s: Array[Char]): List[Char] = {
    var finalString = List[Char]()
    var i = 0
    while (i < s.length) {
      if (s(i) == '[' && i + 4 < s.length && s(i + 4) == ']') {
        finalString :+= '('
        finalString ++= syntaxSugar(s(i + 1), s(i + 3))
        finalString :+= ')'
        i += 4
        if (i + 1 < s.length && !op.contains(s(i + 1)) && !leftOp.contains(s(i + 1))) {
          finalString :+= concatChar
        }
      }
      else {
        finalString :+= s(i)
        if (s(i) == '\'' && i + 2 < s.length) {
          finalString :+= s(i + 1)
          finalString :+= s(i + 2)
          i += 2
        }

        val (condition1, condition2, condition3, condition4) = setConditionsForConcat(s, i)

        if (i + 1 < s.length && condition1 && condition2 && condition3 && condition4) {
          finalString :+= concatChar
        }
      }

      i += 1
    }

    finalString
  }

  private def setConditionsForConcat(s: Array[Char], i: Int) = {
    //Se pune concat daca char la pozitia curenta si la pozitia +1 nu sunt operatii
    val condition1 = !op.contains(s(i)) && i + 1 < s.length && !op.contains(s(i + 1))
    //Daca la pozitie curenta este ( nu se pune concat
    val condition2 = s(i) != '('
    //Nu se pune concat daca este un char si )
    val condition3 = !(!op.contains(s(i)) && i + 1 < s.length && s(i + 1) == ')')
    //Nu se pune concat daca este un char si urmeaza *,+,?
    val condition4 = !(!op.contains(s(i)) && i + 1 < s.length && leftOp.contains(s(i + 1)))
    (condition1, condition2, condition3, condition4)
  }

  def initMaps(): Unit = {
    precedence ++= Map[Char, Int]('(' -> 4, ')' -> 4)
    precedence ++= Map[Char, Int]('+' -> 3, '*' -> 3, '?' -> 3)
    precedence += (concatChar -> 2)
    precedence += ('|' -> 1)

    opToString ++= Map[Char, String]('*' -> "STAR", '?' -> "MAYBE", '+' -> "PLUS")
    opToString ++= Map[Char, String]('|' -> "UNION", concatChar -> "CONCAT")
  }

  def isOperand(i: Char): Boolean = {
    if (i == '(' || i == ')')
      return false
    !op.contains(i) && !leftOp.contains(i)
  }

  def popTillOpenBracket(stack: mutable.Stack[Char]): List[String] = {
    var topChar = stack.top
    var str = List[String]()

    while (topChar != ')') {
      str = str :+ opToString(stack.pop())
      topChar = stack.top
    }

    stack.pop()
    str
  }

  def popTillLowerPrecedence(stack: mutable.Stack[Char], currentChar: Char): List[String] = {
    var topChar = stack.top
    var str = List[String]()

    while (precedence(currentChar) < precedence(topChar) && stack.nonEmpty && topChar != ')') {
      str = str :+ opToString(stack.pop())

      if (stack.nonEmpty)
        topChar = stack.top
    }

    str
  }

  def regexToPrenex(preprocessedString: List[Char]): String = {
    val infix = preprocessedString.reverse
    val stack = mutable.Stack[Char]()
    var prenex = List[String]()

    for ((i, index) <- infix.zipWithIndex) {
      val escapedCharacter = !isOperand(i) && index - 1 >= 0 && infix(index - 1) == '\'' && index + 1 < infix.length && infix(index + 1) == '\'' && List[Char]('*', '+', '(', ')').contains(i)
      if (isOperand(i) || escapedCharacter) {
        prenex = prenex :+ (i + "")
      }
      else if (i == ')') {
        stack.push(i)
      }
      else if (i == '(') {
        prenex ++= popTillOpenBracket(stack)
      } else {
        if (stack.isEmpty) {
          stack.push(i)
        }
        else {
          val topStack = stack.top

          if (precedence(i) > precedence(topStack) || topStack == ')') {
            stack.push(i)
          }
          else if (precedence(i) == precedence(topStack)) {
            stack.push(i)
          }
          else if (precedence(i) < precedence(topStack)) {
            prenex ++= popTillLowerPrecedence(stack, i)
            stack.push(i);
          }
        }
      }
    }

    while (stack.nonEmpty) {
      prenex = prenex :+ opToString(stack.pop())
    }

    prenex = prenex.reverse
    //    prenex.mkString(" ")
    Utils.makeString(prenex)
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    val keyWord = List("eps", "void")
    if (keyWord.contains(str))
      return str

    val preprocessedString = preprocess(str.toCharArray)
    initMaps()
    val s = regexToPrenex(preprocessedString)
    s
  }

  def main(args: Array[String]): Unit = {
    //    println(preprocess("([0-9]*|b+)c?d(da)(\' \'?[A-Z]|\'a\'+)?".toCharArray))
    toPrenex("'\n'")

    var s: String = "'\\n'"
    s = s.replaceFirst("\n", "\n")
    var i = """NEW \n"""
  }


}

import scala.collection.immutable.HashMap

case class Lexer(spec: String) {
  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */

  var regex: Map[String, String] = HashMap[String, String]()
  var priority: Map[String, Integer] = HashMap[String, Integer]()
  var priorityIndex: Integer = 0
  var lastSpec: String = ""
  var lastFinalPosition: Integer = -1
  var startPosition: Integer = 0
  var index = 0
  var line = 0
  var character = 0

  def initData(): Unit = {
    val specifications = spec.split('\n')

    specifications.foreach(str => {
      val splitSpec = str.split(": ")

      splitSpec(1) = splitSpec(1).replace("""\n""", "\n")
      splitSpec(1) = splitSpec(1).replace("""\t""", "\t")

      regex += (splitSpec(0) -> splitSpec(1).substring(0, splitSpec(1).length - 1))
      priority += (splitSpec(0) -> priorityIndex)
      priorityIndex += 1
    })
  }

  def initDFA(): List[(String, Int, Dfa[Int])] = {
    var DFAs = List[(String, Int, Dfa[Int])]()

    regex foreach {
      case (spec, regex) => {
        val dfa = Dfa.fromPrenex(Regex.toPrenex(regex))
        DFAs :+= (spec, dfa.startState, dfa)
      }
    }

    DFAs
  }

  def checkForFinalState(data: (String, Int, Dfa[Int])): Unit = {
    if (!data._3.isFinal(data._2))
      return

    val checkLength = lastSpec.isEmpty || index > lastFinalPosition
    val checkPriorityForEqualLength = index == lastFinalPosition && priority(data._1) < priority(lastSpec)

    if (checkLength || checkPriorityForEqualLength) {
      lastSpec = data._1
      lastFinalPosition = index
    }
  }

  def checkDFAs(DFAs: List[(String, Int, Dfa[Int])], currentChar: Char): List[(String, Int, Dfa[Int])] = {
    var newListDfa = List[(String, Int, Dfa[Int])]()

    DFAs.foreach(dfa => {
      val state = dfa._3.next(dfa._2, currentChar)

      if (!dfa._3.isSink(state)) {
        val data = (dfa._1, state, dfa._3)
        newListDfa :+= data

        checkForFinalState(data)
      }
    })

    newListDfa
  }

  def getLineAndChar(word: String): Unit = {
    var str = word.substring(0, index + 1)
    line = str.count(_ == '\n')

    str = str.reverse
    if (line == 0) {
      character = index
    } else {
      character = str.substring(0, str.indexOf('\n')).length - 1
    }
  }

  def lex(word: String): Either[String, List[(String, String)]] = {
    var result = List[(String, String)]()
    initData()
    val DFAs = initDFA()
    var checkedDFAs = DFAs

    while (startPosition < word.length) {
      val currentChar = word(index)

      checkedDFAs = checkDFAs(checkedDFAs, currentChar)
      getLineAndChar(word)
      index += 1

      checkForErrors(word, checkedDFAs) match {
        case Some(toReturn) => return toReturn
        case None =>
      }

      if (checkedDFAs.isEmpty || index >= word.length) {
        result :+= (word.substring(startPosition, lastFinalPosition + 1), lastSpec)

        startPosition = lastFinalPosition + 1
        lastFinalPosition = -1
        lastSpec = ""

        index = startPosition
        checkedDFAs = DFAs
      }
    }

    Right(result)
  }

  private def checkForErrors(word: String, DFAs: List[(String, Int, Dfa[Int])]): Option[Left[String, Nothing]] = {
    if (DFAs.nonEmpty && index >= word.length && lastFinalPosition == -1) {
      return Some(Left("No viable alternative at character EOF, line " + line))
    }

    if (DFAs.isEmpty && lastFinalPosition == -1) {
      return Some(Left("No viable alternative at character " + character + ", line " + line))
    }
    None
  }
}
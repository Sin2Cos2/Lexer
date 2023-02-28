import Dfa.fromPrenex

import scala.collection.mutable

class Nfa[A](val startState: A) {

  var edges: mutable.Map[A, Array[(String, A)]] = mutable.Map[A, Array[(String, A)]]()
  var nodes: Set[A] = Set[A]()
  var finalNodes: mutable.Set[A] = mutable.Set[A]()

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.
  def map[B](f: A => B): Nfa[B] = {
    val resNFA = new Nfa[B](f(startState))

    for ((key, value) <- this.edges) {
      var mappedArray = Array[(String, B)]()
      value.foreach {
        case (str, neigh) => mappedArray = mappedArray :+ (str, f(neigh))
      }

      resNFA.edges += (f(key) -> mappedArray)
      resNFA.nodes = this.nodes.map(f(_))
      resNFA.finalNodes = this.finalNodes.map(f(_))
    }

    resNFA
  }

  def next(state: A, c: Char): Set[A] = {
    var res: Set[A] = Set[A]()

    def recursiveNext(state: A, c: Char): Unit = {
      if (res.contains(state))
        return

      val neighbors = this.edges(state)

      neighbors.foreach {
        case (character, neighbor) =>
          if (character.equalsIgnoreCase("eps"))
            recursiveNext(neighbor, c)
          else if (character.charAt(0) == c)
            res += neighbor
      }
    }

    recursiveNext(state, c)
    res
  }

  def wayToFinal(state: A): Boolean = {
    var visited: Set[A] = Set[A]()
    var result = false

    def recursiveToFinal(state: A): Unit = {
      if (visited.contains(state))
        return

      val neighbors = this.edges(state)
      neighbors.foreach {
        case (character, neighbor) =>
          if (this.isFinal(neighbor))
            result ||= true
          else if (character.equalsIgnoreCase("eps")) {
            recursiveToFinal(neighbor)
            visited += neighbor
          }
      }
    }

    recursiveToFinal(state)
    result
  }

  def accepts(str: String): Boolean = {
    val splitStr = str.toCharArray
    var i = 0
    var result = false

    def recursionAccept(state: A): Unit = {
      val wordIsConsumed = i >= splitStr.length

      if (this.isFinal(state) && wordIsConsumed) {
        result ||= true
        return
      } else if (this.isFinal(state) && !wordIsConsumed) {
        return
      }

      var charToFind = " ".charAt(0)
      if (!wordIsConsumed)
        charToFind = splitStr(i)
      val nextStates = this.next(state, charToFind)
      i += 1
      for (elem <- nextStates) {
        recursionAccept(elem)
      }

      if (wayToFinal(state) && wordIsConsumed)
        result ||= true
    }

    if (this.nodes.isEmpty)
      return result

    recursionAccept(startState)
    result
  }

  def getStates: Set[A] = this.nodes

  def isFinal(state: A): Boolean = this.finalNodes.contains(state)
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {

  def createNFA(AST: Tree[String]): Nfa[Int] = {
    val NFA = new Nfa[Int](0)
    var currentNode = 0;

    // Metoda returneaza primul si ultimul nod din automat
    def parseAST(AST: Tree[String]): (Int, Int) = {
      if (AST == null)
        return (-1, -1)

      AST.value match {
        case "CONCAT" => concat(NFA, parseAST, AST)
        case "UNION" =>
          val firstNodeUnion = currentNode
          NFA.nodes += (firstNodeUnion)
          currentNode += 1

          val (startFirstNFA, endFirstNFA) = parseAST(AST.left)
          val (startSecondNFA, endSecondNFA) = parseAST(AST.right)

          val lastNodeUnion = currentNode
          NFA.nodes += (lastNodeUnion)

          val firstUnionConnect = Array(("eps", startFirstNFA), ("eps", startSecondNFA))
          NFA.edges += (firstNodeUnion -> firstUnionConnect)
          NFA.edges += (endFirstNFA -> Array(("eps", lastNodeUnion)))
          NFA.edges += (endSecondNFA -> Array(("eps", lastNodeUnion)))

          currentNode += 1
          (firstNodeUnion, lastNodeUnion)
        case "STAR" =>
          val firstStarNode = currentNode
          NFA.nodes += (firstStarNode)
          currentNode += 1

          val (startNFA, endNFA) = parseAST(AST.left)

          val lastStarNode = currentNode
          currentNode += 1
          NFA.nodes += (lastStarNode)

          val startStarConnect = Array(("eps", startNFA), ("eps", lastStarNode))
          NFA.edges += (firstStarNode -> startStarConnect)
          val endNFAConnect = Array(("eps", lastStarNode), ("eps", startNFA))
          NFA.edges += (endNFA -> endNFAConnect)
          (firstStarNode, lastStarNode)
        case atomic =>
          // Create nodes
          atomicNode(NFA, currentNode, atomic)
          currentNode += 2
          (currentNode - 2, currentNode - 1)
      }
    }

    parseAST(AST)
    if (NFA.nodes.nonEmpty) {
      NFA.edges += (NFA.nodes.max -> Array())
      NFA.finalNodes += NFA.nodes.max
    }

    NFA
  }

  private def atomicNode(NFA: Nfa[Int], currentNode: Int, atomic: String): Unit = {
    if (atomic.equalsIgnoreCase("void")) {
      return
    }

    NFA.nodes += (currentNode)
    NFA.nodes += (currentNode + 1)
    NFA.edges += (currentNode -> Array((atomic, currentNode + 1)))
  }

  private def concat(NFA: Nfa[Int], parseAST: Tree[String] => (Int, Int), AST: Tree[String]): (Int, Int) = {
    val (retFirst, last) = parseAST(AST.left)
    val (first, retLast) = parseAST(AST.right)

    var edgesForNode: Array[(String, Int)] = Array[(String, Int)]()
    if (NFA.edges.contains(last)) {
      edgesForNode = edgesForNode ++ NFA.edges(last)
    }

    NFA.edges += (last -> (edgesForNode :+ ("eps", first)))
    (retFirst, retLast)
  }

  def fromPrenex(str: String): Nfa[Int] = {
    val AST = Tree.createAbstractSyntaxTree(Utils.separator(str))

    createNFA(AST)
  }
}
import scala.annotation.tailrec
import scala.collection.immutable.{Map, Set}
import scala.collection.mutable

class Dfa[A](val startState: A, val sinkState: A) {

  var edges: mutable.Map[A, Array[(String, A)]] = mutable.Map[A, Array[(String, A)]]()
  var nodes: Set[A] = Set[A]()
  var finalNodes: mutable.Set[A] = mutable.Set[A]()
  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B): Dfa[B] = {
    val resDfa = new Dfa[B](f(startState), f(sinkState))

    for ((key, value) <- this.edges) {
      var mappedArray = Array[(String, B)]()
      value.foreach {
        case (str, neigh) => mappedArray = mappedArray :+ (str, f(neigh))
      }

      resDfa.edges += (f(key) -> mappedArray)
    }
    resDfa.nodes = this.nodes.map(f(_))
    resDfa.finalNodes = this.finalNodes.map(f(_))

    resDfa
  }

  def next(state: A, c: Char): A = {
    if (!this.edges.contains(state))
      return sinkState
    val neighbors = this.edges(state)

    for (elem <- neighbors) {
      if (elem._1.charAt(0) == c)
        return elem._2
    }

    sinkState
  }

  def accepts(str: String): Boolean = {
    var result = false

    @tailrec
    def recursiveAccept(state: A, str: String): Unit = {
      if (str.isEmpty && isFinal(state)) {
        result ||= true
        return
      }
      if (str.isEmpty)
        return

      val neighbor = next(state, str.head)
      if (neighbor == sinkState)
        return

      recursiveAccept(neighbor, str.tail)
    }

    recursiveAccept(startState, str)
    result
  }

  def getStates: Set[A] = this.nodes

  def isFinal(state: A): Boolean = this.finalNodes.contains(state)

  def isSink(state: A): Boolean = this.sinkState == state
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {

  // Un caracter a fost consumat si mergem prin toate eps posibile
  def epsClosure(nfa: Nfa[Int], startState: Int): Set[Int] = {
    var resSet = Set[Int]()

    def recursiveClosure(state: Int): Unit = {
      if (resSet.contains(state))
        return
      resSet += state

      if (!nfa.edges.contains(state))
        return
      val neighbors = nfa.edges(state)

      neighbors foreach {
        case (character, neigh) =>
          if (character.equalsIgnoreCase("eps")) {
            recursiveClosure(neigh)
          }
      }
    }

    recursiveClosure(startState)
    resSet
  }

  def containsFinalState(nfa: Nfa[Int], nextStateForChar: Set[Int]): Boolean = {
    var result = false
    nextStateForChar.foreach(i => result ||= nfa.isFinal(i))

    result
  }

  def toDfa(nfa: Nfa[Int], language: Set[String]): Dfa[Int] = {
    val startState = epsClosure(nfa, nfa.startState)
    val dfa = new Dfa[Set[Int]](startState, Set(-1))
    dfa.nodes += startState
    if (containsFinalState(nfa, startState))
      dfa.finalNodes += startState

    def recursiveToDfa(state: Set[Int]): Unit = {
      if (dfa.edges.contains(state))
        return

      for (char <- language) {
        var nextStateForChar = Set[Int]()
        state.foreach(i => {
          var neighbors = Array[(String, Int)]()
          if (nfa.edges.contains(i))
            neighbors = nfa.edges(i)
          neighbors foreach {
            case (character, neigh) =>
              if (character == char) {
                // Am consumat caracterul
                nextStateForChar += neigh
                // Epsilon closure pentru starea gasita
                nextStateForChar ++= epsClosure(nfa, neigh)
              }
          }
        })

        if (nextStateForChar.nonEmpty) {
          // Adaugam in set de stari
          dfa.nodes += nextStateForChar
          // Daca este cel putin o stare finala, adaugam in set
          if (containsFinalState(nfa, nextStateForChar))
            dfa.finalNodes += nextStateForChar

          var edges: Array[(String, Set[Int])] = Array[(String, Set[Int])]()
          if (dfa.edges.contains(state))
            edges = dfa.edges(state)

          edges = edges :+ (char, nextStateForChar)
          dfa.edges += (state -> edges)
          recursiveToDfa(nextStateForChar)
        }
      }
    }

    recursiveToDfa(startState)
    var hash = Map[Set[Int], Int]()
    var i = 0
    dfa.map(set => {
      if (!hash.contains(set)) {
        hash += (set -> i)
        i += 1
      }

      hash(set)
    })
  }

  def fromPrenex(str: String): Dfa[Int] = {
    val nfa = Nfa.fromPrenex(str)
    val op = List("UNION", "STAR", "CONCAT", "PLUS", "MAYBE", "eps")
    val language = Utils.separator(str).filter(!op.contains(_)).toSet

    Dfa.toDfa(nfa, language)
  }
}

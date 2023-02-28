import scala.annotation.tailrec
import scala.collection.mutable

class Tree [A](var value: A, var left: Tree[A], var right: Tree[A]){
}

object Tree {
  def createAbstractSyntaxTree(str: Array[String]): Tree[String] = {
    var i = 0
    val operations = List("UNION", "STAR", "CONCAT", "PLUS", "MAYBE")
    val oneLeafOperation = List("STAR", "PLUS", "MAYBE")
    val stack = mutable.Stack[Tree[String]]()

    @tailrec
    def closeOp(): Unit = {
      val checkParsed: Tree[String] = stack.pop()

      val isAtom = !operations.contains(checkParsed.value)
      val oneNoNull = checkParsed.left != null || checkParsed.right != null
      val oneLeafOpParsed = oneLeafOperation.contains(checkParsed.value) && oneNoNull
      val isParsedOp = checkParsed.left != null && checkParsed.right != null || oneLeafOpParsed

      if ((isAtom || isParsedOp) && stack.nonEmpty) {
        var op = stack.pop()

        if (op.value.equalsIgnoreCase("PLUS")) {
          val star = new Tree[String]("STAR", checkParsed, null)
          op = new Tree[String]("CONCAT", checkParsed, star)
        } else if (op.value.equalsIgnoreCase("MAYBE")) {
          val eps = new Tree[String]("eps", null, null)
          op = new Tree[String]("UNION", checkParsed, eps)
        }

        if (op.left == null) {
          op.left = checkParsed;
        } else if (op.right == null) {
          op.right = checkParsed;
        }

        stack.push(op)
      } else {
        stack.push(checkParsed)
        return
      }

      closeOp()
    }

    @tailrec
    def parseString(): Tree[String] = {
      closeOp()

      if (i < str.length) {
        stack.push(new Tree[String](str(i), null, null))
        i += 1
        parseString()
      } else {
        stack.pop()
      }
    }

    stack.push(new Tree[String](str(i), null, null))
    i += 1
    parseString()
  }
}

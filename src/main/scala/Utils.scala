object Utils {
  def separator(str: String): Array[String] = {
    var concat = ""
    var result: Array[String] = Array[String]()
    var i = 0

    while (i < str.length) {
      var temp = str(i)
      if (str(i) == ' ' && concat.nonEmpty) {
        result = result :+ concat
        concat = ""
      }
      else if (str(i) == '\'' && i + 2 < str.length && str(i + 2) == '\'') {
        if (concat.nonEmpty) {
          result = result :+ concat
          concat = ""
        }
        concat += str(i + 1)
        result = result :+ concat
        concat = ""
        i += 2
      }
      else if (str(i) != ' ') {
        concat += str(i)
      }

      i += 1
    }

    if (concat.nonEmpty)
      result = result :+ concat

    result
  }

  def separator(str: String, sep: Char): Array[String] = {
    var concat = ""
    var result: Array[String] = Array[String]()
    var i = 0

    while (i < str.length) {
      var temp = str(i)
      if (str(i) == sep && concat.nonEmpty) {
        result = result :+ concat
        concat = ""
      }
//      else if (str(i) == '\'' && i + 2 < str.length && str(i + 2) == '\'') {
//        concat += str(i + 1)
//        result = result :+ concat
//        concat = ""
//        i += 2
//      }
      else if (str(i) != sep) {
        concat += str(i)
      }

      i += 1
    }

    if (concat.nonEmpty)
      result = result :+ concat

    result
  }

  def makeString(list: List[String]): String = {
    var res = ""
    var i = 0

    while (i < list.size) {
      if (list(i) == "'" && i + 2 < list.size && list(i + 2) == "'") {
        res += list(i) + list(i + 1) + list(i + 2) + " "
        i += 2
      }
      else {
        res += list(i) + " "
      }
      i += 1
    }

    res
  }
}

package compiler

import scala.collection.mutable.ListBuffer

/**
  * Created by MarioDiniz on 10/11/16.
  */
object Proof {
  def isTheorem(premises: List[String], theorem: String): Boolean = {
    var clauses = premises.map(identifyClauses).flatten
    clauses = clauses ++ identifyClauses(Converter.toCNF(s"~(${theorem})"))
    return proof(clauses.sorted)
  }

  private def identifyClauses(premise: String): ListBuffer[String] = {
    val clauses = ListBuffer[String]()
    for (clause <- premise.split('^')) {
      val literals = clause.replace("(", "").replace(")", "").split("v")
      clauses += literals.mkString("v")
    }
    clauses
  }

  private def proof(clauses: List[String]): Boolean = {
    var foundSolution = 0
    var negatedDescendingLiteral: String = null
    var listOfClauses = clauses.toSet

    while (foundSolution == 0) {
      var resolventClauses = ListBuffer[String]()
      var descendingIterator = listOfClauses.iterator.toList.reverse.iterator
      var counterDescending = listOfClauses.size
      while (descendingIterator.hasNext && foundSolution == 0) {
        counterDescending -= 1
        var descendingClause = descendingIterator.next
        var descendingLiteralsList = descendingClause.split("v").map((s) => s.trim).toList
        var ascendingInterator = listOfClauses.iterator
        var counterAscending = -1
        while (ascendingInterator.hasNext && foundSolution == 0) {
          counterAscending += 1
          var ascendingClause = ascendingInterator.next
          if (counterDescending > counterAscending) {
            if (!descendingClause.equals(ascendingClause)) {
              var ascendingLiteralsList = ascendingClause.split("v").map((s) => s.trim).toList
              for (descendingLiteral <- descendingLiteralsList) {
                if (descendingLiteral.startsWith("~")) {
                  negatedDescendingLiteral = descendingLiteral.substring(1)
                } else {
                  negatedDescendingLiteral = s"~${descendingLiteral}"
                }
                if (ascendingLiteralsList.contains(negatedDescendingLiteral)) {
                  var setResolventLiterals = Set[String]()
                  for (ascendingLiteral <- ascendingLiteralsList) {
                    if (!ascendingLiteral.equals(negatedDescendingLiteral)) {
                      setResolventLiterals += ascendingLiteral
                    }
                  }
                  for (descendingLiteralB <- descendingLiteralsList) {
                    if (!descendingLiteralB.equals(descendingLiteral)) {
                      setResolventLiterals += descendingLiteralB
                    }
                  }
                  var isFirst = true
                  var resolvent = ""

                  for (literalResolvent <- setResolventLiterals) {
                    if (!isFirst) {
                      resolvent += "v"
                    } else {
                      isFirst = false
                    }
                    resolvent += literalResolvent
                  }

                  resolventClauses += resolvent

                  if (resolvent.length == 0) {
                    foundSolution = 1
                  }
                }
              }
            }
          }
        }
      }
      if (contains(resolventClauses.toSet, listOfClauses) && foundSolution != 1) {
        foundSolution = -1
      } else {
        listOfClauses ++= resolventClauses
      }
    }

    return foundSolution match {
      case 1 => true
      case _ => false
    }
  }

  private def contains(a: Set[String], b: Set[String]) : Boolean = {
    a.map((x) => b.contains(x)).fold(true)({(x, y) => x && y })
  }
}
package game.client.bashorov

/**
 * @author Zalim Bashorv
 */

object Common {
  type listBoard_t = List[List[List[Int]]]
  type arrayBoard_t = Array[Array[Array[Int]]]

  /**
   * @param board game board
   * @param newPos checking position
   * @param player plyer identefier
   * @return 1 - player wins, 0 - it's a tie, -1 - no winners or opponent player wins
   */
  def checkWinningCondition(board: listBoard_t, newPos: (Int, Int), player: Int): Int = {
    def check(fun: Int => Int, fix: Int): Boolean = {
      for (l <- 0 to 3 if l != fix ) {
        if (fun(l) != player) return false
      }
      true
    }

    val (x, y) =  newPos
    val k = board(x)(y).indexOf(0)
    if (k == -1)
      return -1 //todo

    if (check(board(_)(y)(k), x)) return 1
    if (check(board(x)(_)(k), y)) return 1
    if (check(board(x)(y)(_), k)) return 1

    if (x == y && y == k && check(p => board(p)(p)(p), x)) return 1
    if (x == y && y == 3 - k && check(p => board(p)(p)(3 - p), x)) return 1

    if (x == k && x == 3 - y && check(p => board(p)(3 - p)(p), x)) return 1
    if (y == k && x == 3 - y && check(p => board(p)(3 - p)(3 - p), x)) return 1

    if (x == y     && check(p => board(p)(p)(k), x)) return 1
    if (x == 3 - y && check(p => board(p)(3 - p)(k), x)) return 1

    if (x == k     && check(p => board(p)(y)(p), x)) return 1
    if (x == 3 - k && check(p => board(p)(y)(3 - p), x)) return 1

    if (y == k     && check(p => board(x)(p)(p), y)) return 1
    if (y == 3 - k && check(p => board(x)(p)(3 - p), y)) return 1

    if (board.forall(_.forall(_.forall(_ != 0)))) return 0

    return -1
  }

  def printBoard(board: listBoard_t) {
    for (k <- 0 to 3) {
      for (i <- 0 to 3) {
        for (j <- 0 to 3)
          print(s"\t${board(i)(j)(k)}")
        println()
      }
      println()
    }
  }

  def printBoard(board: arrayBoard_t) {
    for (k <- 0 to 3) {
      for (i <- 0 to 3) {
        for (j <- 0 to 3)
          print(s"\t${board(i)(j)(k)}")
        println()
      }
      println()
    }
  }
}

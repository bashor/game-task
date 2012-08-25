package game.client

/**
 * @author Alefas
 * @since 25.08.12
 */

class StackPlayer(val name: String) extends Player {
  /**
   * @param board 0 - empty field, 1 - your field, 2 - opponent field
   * @return tuple with your turn coordinates (every coordinate should point to range 0 to 3)
   */
  def makeTurn(board: List[List[List[Int]]]): (Int, Int) = {
    for (i <- 0 to 3; j <- 0 to 3) {
      if (board(i)(j).contains(0)) return (i, j)
    }
    (0, 0)
  }
}

package game.client

import game.client.bashorov.Common._

/**
 * @author Zalim Bashorv
 */

class BashorovStackPlayer(val name: String) extends Player {
  /**
   * @param board 0 - empty field, 1 - your field, -1 - opponent field
   * @return tuple with your correct turn coordinates (every coordinate should point to range 0 to 3)
   */
  def makeTurn(board: listBoard_t): (Int, Int) = {
    return findPosition(board)
  }

  def findPosition(board: listBoard_t): (Int, Int) = {
    var ret: Option[(Int, Int)] = None

    var lossPos: Option[(Int, Int)] = None

    for (i <- 0 to 3; j <- 0 to 3) {
      val k = board(i)(j).indexOf(0);
      if (k != -1) {
        if (checkWinningCondition(board, (i, j), 1) == 1)
          return (i,j)

        if (ret.isEmpty)
          ret = Some((i, j))

        if (checkWinningCondition(board, (i, j), -1) == 1)
          lossPos = Some((i, j))
      }
    }

    if (lossPos.isEmpty)
      ret.get
    else
      lossPos.get
  }
}
package game.client

import game.server.GameServer
import util.Random
import compat.Platform.currentTime
import scala.Some

import game.client.bashorov.Common._

/**
 * @author Zalim Bashorv
 */

class BashorovRandomPlayer(val name: String) extends Player {

  /**
   * @param board 0 - empty field, 1 - your field, -1 - opponent field
   * @return tuple with your correct turn coordinates (every coordinate should point to range 0 to 3)
   */
  def makeTurn(board: listBoard_t): (Int, Int) = {
    initEndTime()

    val (pos, firstFree) = findPosition(board)
    if (!pos.isEmpty) {
      return pos.get
    }

    val RETURN_TIME = 10
    var l = 1
    while (true) {
      val (i, j) = (Random.nextInt(4), Random.nextInt(4))
      if (board(i)(j).contains(0)) return (i, j)

      //macrooptimisations :)
      if (l % 1024 == 0 && remainingTime < RETURN_TIME) {
        return firstFree
      }
      l += 1
    }

    firstFree
  }

  private var endTime = 0L
  private def initEndTime() = { endTime = currentTime + GameServer.TIMEOUT }
  private def remainingTime: Long = endTime - currentTime

  private def findPosition(board: listBoard_t): (Option[(Int, Int)], (Int, Int)) = {
    var lossPos: Option[(Int, Int)] = None
    var firstFree: Option[(Int, Int)] = None

    for (i <- 0 to 3; j <- 0 to 3) {
      val k = board(i)(j).indexOf(0);
      if (k != -1) {
        if (firstFree.isEmpty)
          firstFree = Some((i, j))

        if (checkWinningCondition(board, (i, j), 1) == 1)
          return (Some((i,j)), firstFree.get)

        if (checkWinningCondition(board, (i, j), -1) == 1)
          lossPos = Some((i,j))
      }
    }

    (lossPos, firstFree.get)
  }
}

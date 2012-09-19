package game.client

import game.server.GameServer
import scala.compat.Platform.currentTime

import game.client.bashorov.Common._

/**
 * @author Zalim Bashorv
 */

class BashorovBfsPlayer(val name: String) extends Player {
  /**
   * @param board 0 - empty field, 1 - your field, -1 - opponent field
   * @return tuple with your correct turn coordinates (every coordinate should point to range 0 to 3)
   */
  def makeTurn(board: listBoard_t): (Int, Int) = {
    startTime = currentTime
    val pos = findPosition(board)
    if (!pos.isEmpty) {
      return pos.get
    }

    val r = bfs(board)

    if (r.isEmpty)
      return (0, 0)
    else
      return r.get
  }

  private var startTime = 0L
  private val TIMEOUT = GameServer.TIMEOUT - 900
  private def timeIsUp: Boolean = (currentTime - startTime) > TIMEOUT
  private def remainingTime: Long = TIMEOUT - (currentTime - startTime)

  def findPosition(board: listBoard_t): Option[(Int, Int)] = {
    var lossPos: Option[(Int, Int)] = None

    for (i <- 0 to 3; j <- 0 to 3) {
      val k = board(i)(j).indexOf(0);
      if (k != -1) {
        if (checkWinningCondition(board, (i, j), 1) == 1)
          return Some((i,j))

        if (checkWinningCondition(board, (i, j), -1) == 1)
          lossPos = Some((i,j))
      }
    }

    lossPos
  }

  def bfs(board: listBoard_t): Option[(Int, Int)] = {
    var r: Option[(Int, Int)] = None
    val queue = new scala.collection.mutable.Queue[((Int, Int), arrayBoard_t)]

    val arrBoard = board.map(_.map(_.toArray[Int]).toArray[Array[Int]]).toArray[Array[Array[Int]]]

    for (i <- 0 to 3; j <- 0 to 3) {
      val k = board(i)(j).indexOf(0)
      if (k != -1) {
        val modifiedBoard = board.map(_.map(_.toArray[Int]).toArray[Array[Int]]).toArray[Array[Array[Int]]]
        modifiedBoard(i)(j)(k) = 1
        //        printBoard(modifiedBoard)
        queue.enqueue(((i, j), modifiedBoard))
      }
    }

    while (!queue.isEmpty) {
      val (pos, curBoard) = queue.dequeue()
      //      printBoard(curBoard)
      val listBoard = curBoard.map(_.map(_.toList).toList).toList
      if (GameServer.checkWinningCondition(listBoard) == 1) {
        println (s"win $pos")
        printBoard(listBoard)
        return Some(pos)
      }

      def inverseBoard(board: arrayBoard_t) = board.map(_.map(_.map(_ * -1)))

      val opponentBoard = inverseBoard(curBoard)
      for (i <- 0 to 3; j <- 0 to 3) {
        val k = board(i)(j).indexOf(0)
        if (k != -1) {
          opponentBoard(i)(j)(k) = 1
          if (GameServer.checkWinningCondition(listBoard) != 1) {//todo
            queue.enqueue((pos, inverseBoard(opponentBoard)))
            r = Some(pos)
          }
        }
      }

      if (timeIsUp) {
//        println(s"timeIsUp ${currentTime - startTime} $r")
        return r
      }
    }

    r
  }
}

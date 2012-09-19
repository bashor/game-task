package game.client

import game.server.GameServer
import util.Random
import compat.Platform.currentTime
import scala.Some

import game.client.bashorov.Common._

/**
 * @author Zalim Bashorv
 */

class BashorovSmart2Player(val name: String) extends Player {

  /**
   * @param board 0 - empty field, 1 - your field, -1 - opponent field
   * @return tuple with your correct turn coordinates (every coordinate should point to range 0 to 3)
   */
  def makeTurn(board: listBoard_t): (Int, Int) = {
    findPosition(board)
  }

  class Rating(val mine3: Int, val opponent3: Int, val mine2free2: Int, val opponent2free2: Int, val mine1free3: Int, val free4: Int) {
    def +(a: Rating): Rating =
      new Rating(mine3 + a.mine3, opponent3 + a.opponent3, mine2free2 + a.mine2free2,
        opponent2free2 + a.opponent2free2, mine1free3 + a.mine1free3, free4 + a.free4)

    def <(a: Rating) =
      mine3 < a.mine3 ||
        (mine3 == a.mine3 && opponent3 < a.opponent3) ||
        (mine3 == a.mine3 && opponent3 == a.opponent3 && mine2free2 < a.mine2free2) ||
        (mine3 == a.mine3 && opponent3 == a.opponent3 && mine2free2 == a.mine2free2 && opponent2free2 < a.opponent2free2) ||
        (mine3 == a.mine3 && opponent3 == a.opponent3 && mine2free2 == a.mine2free2 && opponent2free2 == a.opponent2free2 && mine1free3 < a.mine1free3) ||
        (mine3 == a.mine3 && opponent3 == a.opponent3 && mine2free2 == a.mine2free2 && opponent2free2 == a.opponent2free2 && mine1free3 == a.mine1free3 && free4 < a.free4)

    override def toString() = s"R($mine3, $opponent3, $mine2free2, $opponent2free2, $mine1free3, $free4)"
  }
  object Rating {
    def zero = new Rating(0, 0, 0, 0, 0, 0)
    def mine3 = new Rating(1, 0, 0, 0, 0, 0)
    def opponent3 = new Rating(0, 1, 0, 0, 0, 0)
    def mine2free2(k: Int) = new Rating(0, 0, 0, k, 0, 0)
    def opponent2free2(k: Int) = new Rating(0, 0, 0, k, 0, 0)
    def mine1free3(k: Int) = new Rating(0, 0, 0, 0, k, 0)
    def free4(k: Int) = new Rating(0, 0, 0, 0, 0, k)
  }

  private def findPosition(board: listBoard_t): (Int, Int) = {

    val rating = Array.fill(4, 4)(Rating.zero)

    def initMaxRating(): (Rating, (Int, Int)) = {
      for (i <- 0 to 3; j <- 0 to 3) {
        val k = board(i)(j).indexOf(0);
        if (k != -1) {
          return (calcRating(board, (i, j)), (i, j));
        }
      }
      (calcRating(board, (0, 0)), (0, 0))
    }

    var (maxRating, maxRatingPos)= initMaxRating()

    for (i <- 0 to 3; j <- 0 to 3) {
      val k = board(i)(j).indexOf(0);
      if (k != -1) {
        val rating = calcRating(board, (i, j))
        if (maxRating < rating) {
          maxRating = rating
          maxRatingPos = (i, j)
        }
      }
    }

    maxRatingPos
  }

  def calcRating(board: listBoard_t, newPos: (Int, Int)): Rating = {
    def check(fun: Int => (Int, Int)): Rating = {
      var (mine, anothers, free, freec) = (0, 0, 0, 0)
      for (l <- 0 to 3) {
        val (v, d) = fun(l)
        if (v == 1)
          mine += 1
        else if (v == 0) {
          free += 1
          freec += 5 - d
        }
        else
          anothers += 1
      }

      if (free == 1) {
        if (mine == 3)
          return Rating.mine3
        else if (anothers == 3)
          return Rating.opponent3
      } else if (free == 2) {
        if (mine == 2 )
          return Rating.mine2free2(freec)
        else if (anothers == 2)
          return Rating.opponent2free2(freec)
      } else if (free == 3) {
        if (mine == 1)
          return Rating.mine1free3(freec)
      } else if (free == 4) {
        return Rating.free4(freec)
      }

      Rating.zero
    }

    val (x, y) =  newPos
    val k = board(x)(y).indexOf(0)
    if (k == -1)
      return Rating.zero //todo

    def f(i: Int, j: Int, l: Int): (Int, Int) = (board(i)(j)(l), l - board(i)(j).indexOf(0) + 1)

    check(f(_, y, k)) +
      check(f(x, _, k)) +
      check(f(x, y, _)) +
      (if (x == y && y == k) check(p => f(p, p, p)) else Rating.zero) +
      (if (x == y && y == 3 - k) check(p => f(p, p, 3 - p)) else Rating.zero) +
      (if (x == k && x == 3 - y) check(p => f(p, 3 - p, p)) else Rating.zero) +
      (if (y == k && x == 3 - y) check(p => f(p, 3 - p, 3 - p)) else Rating.zero) +
      (if (x == y) check(p => f(p, p, k)) else Rating.zero) +
      (if (x == 3 - y) check(p => f(p, 3 - p, k)) else Rating.zero) +
      (if (x == k) check(p => f(p, y, p)) else Rating.zero) +
      (if (x == 3 - k) check(p => f(p, y, 3 - p)) else Rating.zero) +
      (if (y == k) check(p => f(x, p, p)) else Rating.zero) +
      (if (y == 3 - k) check(p => f(x, p, 3 - p)) else Rating.zero)
  }
}

package game.server

import game.client._
import collection.mutable
import java.util.concurrent.{TimeUnit, TimeoutException, Callable, Executors}

/**
 * @author Alefas
 * @since 25.08.12
 */

object GameServer {
  val TIMEOUT = 1000

  private val players: List[Player] =
    List(
//      new RandomPlayer("Random"),
//      new StackPlayer("Stack"),
//      new BashorovStackPlayer("BashorStack"),
//      new BashorovRandomPlayer("BashorRandom"),
//      new BashorovBfsPlayer("BashorBfs"),
      new BashorovSmartPlayer("BashorovSmarty"),
//      new BashorovSmart2Player("BashorovSmarty 2"),
      new KrinkinPlayer("Krinkin")
//      new SergeyLazarevPlayer("Lazarev"),
//      new IvanovakAlpha2("Ivanov")
    )

  def main(args: Array[String]) {
//    for (l <- 1 to 10)
    {
    val results = mutable.HashMap.empty[String, Int]
    val win = mutable.HashMap.empty[String, Int]
    val loss = mutable.HashMap.empty[String, Int]
    val tie = mutable.HashMap.empty[String, Int]

    for (player <- players) {
      results += ((player.name, 0))
      win += ((player.name, 0))
      loss += ((player.name, 0))
      tie += ((player.name, 0))
    }
//    for (l <- 1 to 5)
    for (first <- players; second <- players if first ne second) {
      val gameResult = game(first, second)
      if (gameResult == 0) {
//        println(s"tie $first $second")
        results.update(first.name, results(first.name) + 1)
        results.update(second.name, results(second.name) + 1)
        tie.update(first.name, tie(first.name) + 1)
        tie.update(second.name, tie(second.name) + 1)
      }
      if (gameResult == 1) {
//        println(s"win $first $second")
        results.update(first.name, results(first.name) + 2)
        win.update(first.name, win(first.name) + 1)
        loss.update(second.name, loss(second.name) + 1)
      }
      if (gameResult == -1) {
//        println(s"win $second $first")
        results.update(second.name, results(second.name) + 2)
        win.update(second.name, win(second.name) + 1)
        loss.update(first.name, loss(first.name) + 1)
      }
      println("----")
    }
    results.toList.sortBy(-_._2).foreach {
      case tuple =>
        val (player, score) = tuple
        val k = (win(player), loss(player), tie(player))
        println(s"${player}: ${0.5 * score} $k")
    }
//  println("----")
    }
    System.exit(0)
  }

  private def game(first: Player, second: Player, board: Array[Array[Array[Int]]] = Array.fill(4, 4, 4)(0)): Int = {
    def convertToList: List[List[List[Int]]] = board.map(_.map(_.toList).toList).toList

    //make turn
    val listBoard = convertToList
    val executor = Executors.newSingleThreadExecutor();
    val future = executor.submit(new Callable[(Int, Int)] {
      def call() = first.makeTurn(listBoard)
    })
    val (x, y) =
      try {
        future.get(TIMEOUT, TimeUnit.MILLISECONDS)
      }
      catch {
        case e: TimeoutException => {
          println(s"TIMEOUT :( (${first.name})")
          return -1
        } //forfeiture
      }
    executor.shutdownNow()
    if (!0.to(3).contains(x) || !0.to(3).contains(y)){
      println(s"Bad pos:( ${(x, y)} (${first.name})")
      return -1
    } //forfeiture
    //update board
    val index = board(x)(y).indexOf(0)
    if (index == -1){
      println(s"Bad pos:( ${(x, y)} dosn't free (${first.name})")
      return -1 //forfeiture
    }
    board(x)(y)(index) = 1
    println(s"${first.name}\t${(x, y)}")
    //check board
    val winningCondition = checkWinningCondition(convertToList)
    if (winningCondition >= 0) return winningCondition
    //update board
    for (i <- 0 to 3; j <- 0 to 3; k <- 0 to 3) board(i)(j)(k) = board(i)(j)(k) * -1
    -game(second, first, board)
  }

  /**
   * This method doesn't check player '-1' winning condition
   * @param board game board
   * @return 1 - player with '1' wins, 0 - it's a tie, -1 - no winners or player '-1' wins
   */
  def checkWinningCondition(board: List[List[List[Int]]]): Int = {
    def check(fun: Int => Int): Boolean = {
      for (k <- 0 to 3) {
        if (fun(k) != 1) return false
      }
      true
    }
    for (i <- 0 to 3; j <- 0 to 3) {
      if (check(board(i)(j)(_))) return 1
      if (check(board(i)(_)(j))) return 1
      if (check(board(_)(i)(j))) return 1
    }
    for (i <- 0 to 3) {
      if (check(p => board(i)(p)(p))) return 1
      if (check(p => board(p)(i)(p))) return 1
      if (check(p => board(p)(p)(i))) return 1
      if (check(p => board(i)(p)(3 - p))) return 1
      if (check(p => board(p)(i)(3 - p))) return 1
      if (check(p => board(p)(3 - p)(i))) return 1
    }
    if (check(p => board(p)(p)(p))) return 1
    if (check(p => board(p)(p)(3 - p))) return 1
    if (check(p => board(p)(3 - p)(3 - p))) return 1
    if (check(p => board(p)(3 - p)(p))) return 1
    if (board.forall(_.forall(_.forall(_ != 0)))) return 0
    return -1
  }
}

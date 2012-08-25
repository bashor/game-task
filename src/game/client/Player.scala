package game.client

/**
 * @author Alefas
 * @since 25.08.12
 */

trait Player {
  /**
   * @param board 0 - empty field, 1 - your field, -1 - opponent field
   * @return tuple with your correct turn coordinates (every coordinate should point to range 0 to 3)
   */
  def makeTurn(board: List[List[List[Int]]]): (Int, Int)

  def name: String
}

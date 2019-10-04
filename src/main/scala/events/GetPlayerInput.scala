package v2.events

import v2.GameState

case class GetPlayerInput[E <: Event: Enumerable]() extends Event {
  override def run(state: GameState): GameState = state

  override def nextEvent(state: GameState): Event = {
    val possibleMoves: Seq[E] = Enumerable[E].allValidMoves(state)
    println("possible moves: " + possibleMoves.mkString("[", ",", "]"))
    possibleMoves.size match {
      case 0 =>
        println("No moves possible")
        println("all moves: ")
        Enumerable[E].allPossibleMoves.mkString("[", "|", "]")
        throw new IllegalArgumentException("Waiting for player move when none are possible")
      case 1 =>
        println(s"Only one move is possible, selecting it automatically: ${possibleMoves.head}")
        possibleMoves.head
      case _ =>
        val move = state.constants.rng.shuffle(possibleMoves).head
        println(s"Player ${state.currentPlayer}: $move")
        move
    }
  }
}


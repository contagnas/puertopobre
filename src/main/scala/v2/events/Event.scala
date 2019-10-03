package v2.events

import simulacrum.typeclass
import v2.GameState

trait Event {
  def validationError(state: GameState): Option[String] = None
  def run(state: GameState): GameState
  def nextEvent(state: GameState): Event
}

@typeclass trait Enumerable[E <: Event] {
  def allValidMoves(state: GameState): Seq[E] =
    allPossibleMoves.filter {
      move => move.validationError(state).isEmpty
    }

  def allPossibleMoves: Seq[E] =
    throw new NotImplementedError("You need to implement at least one method in Enumerable")
}

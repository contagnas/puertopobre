package v2

import v2.events.{Event, GameOver, GetPlayerInput, SelectRole}

object Main extends App {
  val seed = System.currentTimeMillis().toInt
  val initialState = GameState.initialState( 3, seed)

  val eventNumber = LazyList.continually(1)
      .scanLeft(0)(_ + _)

  eventNumber
    .scanLeft[(GameState, Event)]((initialState, GetPlayerInput[SelectRole])) {
      case ((state, event), i) =>
        val nextState = event.run(state)
        val playerOwnedColonists = nextState.players.map(_.colonists.total).sum
        val totalColonists = playerOwnedColonists + nextState.colonistsOnShip + nextState.colonistsInSupply
        if (totalColonists != 55)
          throw new Exception(s"Not 55 colonists at event=$i")
        val nextEvent = event.nextEvent(nextState)
        (nextState, nextEvent)
    }.find {
    case (lastState, GameOver()) =>
      println("game over")
      println(s"seed = ${seed}")
      println(lastState)
      true
    case _ => false
  }
}

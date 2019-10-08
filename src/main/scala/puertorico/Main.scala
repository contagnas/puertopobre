package puertorico

import puertorico.events.{Event, GameOver, GetPlayerInput, SelectRole}

object Main extends App {

  val eventNumber = LazyList.continually(1)
      .scanLeft(0)(_ + _)

  while (true) {
    val now = System.currentTimeMillis()

    val seed = System.currentTimeMillis().toInt
    val initialState = GameState.initialState( 3, seed)
    println(s"seed = ${seed}")

    eventNumber
      .scanLeft[(GameState, Event)]((initialState, GetPlayerInput[SelectRole])) {
        case ((state, event), i) =>
          val nextState = event.run(state)
          val nextEvent = event.nextEvent(nextState)
          (nextState, nextEvent)
      }.find {
      case (lastState, GameOver()) =>
        true
      case _ => false
    }
    val now2 = System.currentTimeMillis()
    println(s"time to run = ${now2 - now} ms, seed = $seed")
  }
}

import Event.{GameOver, GetPlayerInput, SelectRole}

object Main extends App {
  val players = 3

  val stream = Stream.continually(null)
    .scanLeft[(GameState, Event)](
      GameState.initialState(players),
      GetPlayerInput[SelectRole]
    ) { case ((gameState, event), _) => GameEngine.step(gameState, event)
    }.find { case (_, event) => event == GameOver }
}

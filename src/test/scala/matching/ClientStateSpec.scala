package matching

import matching.protocol.ClientState
import org.scalatest.FlatSpec

class ClientStateSpec extends FlatSpec {

  /**
    * Тестирует соответствие результатов методов [[ClientState]] `stringify` и `fromString`
    */
  "A ClientState #fromString and #stringify" should "satisfy contract" in {
    val clientState = ClientState("C1", 100, Map("A" -> 10, "B" -> 20, "C" -> 30, "D" -> 40))
    val values = Array("C1", 100, 10, 20, 30, 40)
    val stringExpected = values.mkString(ClientState.delimiter.toString)
    val stringActual = ClientState.stringify(clientState)
    assert(stringExpected == stringActual)

    val clientStateActual = ClientState.fromString(stringExpected)
    assert(clientStateActual == clientState)
  }
}

package matching

import matching.protocol.{Buy, Order}
import org.scalatest.FlatSpec

class OrderSpec extends FlatSpec {

  /**
    * Тестирует результат метода [[matching.protocol.Order]] `fromString`
    */
  "A Order #fromString" should "return valid object" in {
    val expected = Order(1, "C1", Buy, "A", 1, 50)
    val values = Array("C1", "b", "A", 1, 50)
    val stringExpected = values.mkString(Order.delimiter.toString)
    val actual = Order.fromString(stringExpected)
    assert(expected == actual)
  }
}

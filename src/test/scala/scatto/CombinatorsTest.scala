package scatto

import org.scalatest._
import scatto.parser.Combinators

class CombinatorsTest extends FlatSpec with Matchers {
  "satisfy" should "parse correct characters" in {
    Combinators.satisfy(x => x == 'a')("a") shouldEqual List(('a', ""))
  }

  it should "fail on incorrect characters" in {
    Combinators.satisfy(x => x == 'a')("b") shouldEqual List()
  }
}

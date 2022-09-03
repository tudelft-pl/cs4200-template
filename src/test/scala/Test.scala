//test: Test

import org.scalatest.FunSuite

class Test extends FunSuite {

  import Solution._
  import Library._
  import Lint._
  import Reader.read

  test("Example test") {
    assertResult(
      42
    ) {
      meaning_of_life()
    }
  }
}

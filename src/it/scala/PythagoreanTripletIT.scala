import org.scalatest.{FunSuite, Matchers}

class PythagoreanTripletIT extends FunSuite with Matchers {

  test("findSolution") {
    PythagoreanTriplet.findSolution(60) should be (List(Solution((10,24,26),6240), Solution((15,20,25),7500)))
  }
}
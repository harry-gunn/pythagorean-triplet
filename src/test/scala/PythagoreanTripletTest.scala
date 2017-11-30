import org.scalatest.{FunSuite, Matchers}

class PythagoreanTripletTest extends FunSuite with Matchers {
  test("isPythagorean") {
    PythagoreanTriplet.isPythagorean((3, 4, 5)) shouldBe true
    PythagoreanTriplet.isPythagorean((3, 5, 4)) shouldBe true
    PythagoreanTriplet.isPythagorean((4, 3, 5)) shouldBe true
    PythagoreanTriplet.isPythagorean((4, 5, 3)) shouldBe true
    PythagoreanTriplet.isPythagorean((5, 3, 4)) shouldBe true
    PythagoreanTriplet.isPythagorean((5, 4, 3)) shouldBe true
    PythagoreanTriplet.isPythagorean((3, 3, 3)) shouldBe false
    PythagoreanTriplet.isPythagorean((5, 6, 7)) shouldBe false
  }

  test("pythagoreanTriplets 1 to 10") {
    PythagoreanTriplet.pythagoreanTriplets(1, 10) should be (Seq((3, 4, 5), (6, 8, 10)))
  }

  test("pythagoreanTriplets 11 to 20") {
    PythagoreanTriplet.pythagoreanTriplets(11, 20) should be (Seq((12, 16, 20)))
  }

  test("pythagoreanTriplets 56 to 95") {
    PythagoreanTriplet.pythagoreanTriplets(56, 95) should be (Seq((57, 76, 95), (60, 63, 87)))
  }

  test("sum triplets") {
    PythagoreanTriplet.sumOfTriplets((3, 4, 5)) shouldBe 12
    PythagoreanTriplet.sumOfTriplets((6, 8, 10)) shouldBe 24
    PythagoreanTriplet.sumOfTriplets((12, 16, 20)) shouldBe 48
    PythagoreanTriplet.sumOfTriplets((57, 76, 95)) shouldBe 228
    PythagoreanTriplet.sumOfTriplets((60, 63, 87)) shouldBe 210
  }

  test("product of triplets") {
    PythagoreanTriplet.productOfTriplets((3, 4, 5)) shouldBe 60
    PythagoreanTriplet.productOfTriplets((6, 8, 10)) shouldBe 480
    PythagoreanTriplet.productOfTriplets((12, 16, 20)) shouldBe 3840
    PythagoreanTriplet.productOfTriplets((57, 76, 95)) shouldBe 411540
    PythagoreanTriplet.productOfTriplets((60, 63, 87)) shouldBe 328860
  }

}

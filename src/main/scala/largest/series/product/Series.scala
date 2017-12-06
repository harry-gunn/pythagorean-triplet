package largest.series.product

object Series {


  def largestProduct(span: Int, inputString: String) = {
    val indexLastStartChar = inputString.length - span

    def checkSpanIsGoodLength = span >= 0 && inputString.length >= span
    def checkInputHasNoBadChars = inputString.matches("[0-9]*")

    if(checkSpanIsGoodLength && checkInputHasNoBadChars) {
      val products = for {
        i <- 0 to indexLastStartChar
        substr = inputString.substring(i, i + span)
        x <- getProduct(substr.toString)
      } yield x
      Some(products.max)
    } else None
  }

  def getProduct(numberString: String): List[Int] = {
    List(numberString.map(_.asDigit).foldLeft(1)(_ * _))
  }

}

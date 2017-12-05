package say


object Say extends App {

  def inEnglish(number: Long): Option[String] = {
    number match {
      case x: Long if x < 0 => None
      case x: Long if x > 999999999999l => None
      case 0 => Some("zero")
      case _ => Some(buildAnswer(number))
    }
  }

  def buildAnswer(number: Long): String = {
    val splitNumberIntoThreeDigitGroups: List[Int] = {
      number.toString.reverse.grouped(3).toList.map(x => x.reverse).reverse.map(x => x.toInt)
    }

    def getStringOfHundred(numberGroup: Int, groupPositionInFullNumber: Int) = {

      val numberGroupDescriptor = if(numberGroup == 0) "" else NumberDictionary.groupSize(groupPositionInFullNumber)
      val reversedNumberString = numberGroup.toString.reverse
      val isDivisibleByTen: Boolean = numberGroup % 10 == 0

      def getHundred = reversedNumberString.substring(2).toInt
      def getTen = reversedNumberString.substring(1, 2).toInt
      def getDigit = reversedNumberString.substring(0, 1).toInt

      def getDigitString(x: Int): String = NumberDictionary.digits(x)

      def getTensString: String = {
        s"${
          NumberDictionary.multiplesOfTen(getTen)
        }${
          if (isDivisibleByTen) "" else "-"
        }${
          NumberDictionary.digits(getDigit)
        }"
      }

      def getHundredsString: String = {
        s"${
          NumberDictionary.digits(getHundred)
        } hundred ${getTensString}".trim
      }

      s"""${
        numberGroup match {
          case x: Int if x < 20 => getDigitString(x)
          case x: Int if x < 100 => getTensString
          case x: Int if x < 1000 => getHundredsString
          case _ => ""
        }
      }${numberGroupDescriptor}"""
    }

    val numberInHundreds = splitNumberIntoThreeDigitGroups
    var count = numberInHundreds.length
    numberInHundreds.map(x => getStringOfHundred(x, {count = count - 1; count})).mkString.trim
  }
}

package fintech.homework02
import org.scalatest.{FlatSpec, Matchers}

class ComplexNumberSpec extends FlatSpec with Matchers {
  import ComplexNumber._

  "ComplexNumber" must "compare correctly" in {
    ComplexNumber(11, 13.5) shouldEqual ComplexNumber(11, 13.5)
    3.i should not equal ComplexNumber(1, 3)
  }

  "ComplexNumber" must "add the numbers correctly" in {
    12 + 13.i shouldEqual ComplexNumber(12, 13)
    12 + 0.i shouldEqual ComplexNumber(12)
    (13.9 + 10.6.i) + (10 + 5.i) shouldEqual ComplexNumber(23.9, 15.6)
    13.6 + 5.i + 1000 shouldEqual ComplexNumber(1013.6, 5)
  }

  "ComplexNumber" must "have a toString method" in {
    (13 + 44.i).toString shouldEqual "13.0 + 44.0i"
    15.9.i.toString shouldEqual "0.0 + 15.9i"
  }

  "ComplexNumber" must "multiply correctly" in {
    (13 + 5.i) * 10.i shouldEqual ComplexNumber(-50, 130)
    0 * (4 + 90.i) shouldEqual ComplexNumber.zero
    (2 + 10.i) * (3 + 5.i) shouldEqual ComplexNumber(-44, 40)
  }

  "ComplexNumber" must "divide correctly" in {
    (-50 + 130.i) / (13 + 5.i) shouldEqual ComplexNumber(0, 10)
    (-50 + 130.i) / ComplexNumber.one shouldEqual ComplexNumber(-50, 130)
    assertThrows[ArithmeticException] {
      (-50 + 130.i) / ComplexNumber.zero
    }
  }

  "ComplexNumber" must "exponentiation correctly" in {
    (2 + 10.i) ~ 6 shouldEqual ComplexNumber(-423936, 1041920)
    (3 + 11.i) ~ 1 shouldEqual ComplexNumber(3, 11)
    (3 + 11.i) ~ 0 shouldEqual ComplexNumber.one
    val res = (2 + 10.i) ~ -6
    res.real shouldEqual -3.35042779 * 1E-7 +- 1E-15
    res.imaginary shouldEqual -8.23444511 * 1E-7 +- 1E-15
  }

}

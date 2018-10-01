package fintech.homework02

final case class ComplexNumber(real: Double, imaginary: Double = 0) {

  def +(complex: ComplexNumber): ComplexNumber =
    ComplexNumber(real + complex.real, imaginary + complex.imaginary)

  def +(d: Double): ComplexNumber = ComplexNumber(real + d, imaginary)

  def *(complex: ComplexNumber): ComplexNumber =
    ComplexNumber(real * complex.real - imaginary * complex.imaginary, real * complex.imaginary + complex.real * imaginary)

  def /(complex: ComplexNumber): ComplexNumber = {
    if (complex == ComplexNumber.zero)
      throw new ArithmeticException("denominator = 0")
    val (a, b, c, d) = (real, imaginary, complex.real, complex.imaginary)
    ComplexNumber((a * c + b * d) / (c * c + d * d), (b * c - a * d) / (c * c + d * d))
  }

  // оператор возведения в целую степень.
  def ~(exp: Int): ComplexNumber = {
    if (exp == 0)
      return ComplexNumber.one
    var re = real
    var im = imaginary
    for (_ <- 2 to (if (exp > 0) exp else -exp)) {
      val reCurrent = re * real - im * imaginary
      val imCurrent = re * imaginary + real * im
      re = reCurrent
      im = imCurrent
    }
    if (exp > 0) ComplexNumber(re, im)
    else ComplexNumber.one / ComplexNumber(re, im)
  }

  override def toString: String = s"$real + ${imaginary}i"
}

object ComplexNumber {
  val zero = ComplexNumber(0)
  val one = ComplexNumber(1)

  implicit def toComplex(value: Double): ComplexNumber = ComplexNumber(value)

  implicit class ReachDouble(private val value: Double) extends AnyVal {
    def i: ComplexNumber = ComplexNumber(0, value)
  }

}

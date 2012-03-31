import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

object Fraction {
  def apply(n: Int, d: Int) = new Fraction(n, d)
  def unapply(f: Fraction): Option[(Int, Int)] = Some(f.n, f.d)

  def gcd(n: Int, d: Int): Int = if(d == 0) Math.abs(n) else gcd(d, n % d)
}

class Fraction(val n: Int, val d: Int) {
  override def equals(other: Any): Boolean = {
    other match {
      case that: Fraction => 
        val thisReducded = this.reduce
        val thatReduced = that.reduce
        thisReducded.n == thatReduced.n && thisReducded.d == thatReduced.d

      case _ => false
    }
  }

  lazy val gcd = Fraction.gcd(n, d)
  def reduce = if(gcd == 0) this else Fraction(n/gcd, d/gcd)

  def *(other: Fraction) = Fraction(this.n * other.n, this.d * other.d).reduce
  def +(other: Fraction) = Fraction(this.n * other.d + other.n * this.d, this.d * other.d).reduce
  def -(other: Fraction) = Fraction(this.n * other.d - other.n * this.d, this.d * other.d).reduce
  def /(other: Fraction) = Fraction(this.n * other.d, other.n * this.d).reduce
  override def toString = "%d/%d" format(n, d)
}

class FractionSpec extends FlatSpec with ShouldMatchers {
  it should "allow to extract the nominator and denominator" in {
    val Fraction(n, d) = Fraction(1, 2)
    n should be === 1
    d should be === 2
  }

  it should "calculate the gcd" in {
    Fraction.gcd(120, 180) should be === 10
    Fraction.gcd(2, 4) should be === 2
    Fraction.gcd(35, 100) should be === 5
  }

  it should "reduce a fraction" in {
    Fraction(2, 4).reduce should be === Fraction(1, 2)
    Fraction(120, 180).reduce should be === Fraction(12, 18)
  }
}

class Chapter11 extends FlatSpec with ShouldMatchers {
  "Exercise 3" should "implement a fraction class with operations + - / *" in {
    (Fraction(1, 2) * Fraction(3, 4)) should be === Fraction(3, 8)
    (Fraction(2, 2) * Fraction(2, 2)) should be === Fraction(1, 1)
    (Fraction(1, 2) * Fraction(2, 3)) should be === Fraction(1, 3)

    (Fraction(1, 2) + Fraction(1, 2)) should be === Fraction(1, 1)
    (Fraction(3, 8) + Fraction(2, 4)) should be === Fraction(7, 8)
    (Fraction(1, 3) + Fraction(1, 2)) should be === Fraction(5, 6)

    (Fraction(1, 2) - Fraction(1, 2)) should be === Fraction(0, 1)
    (Fraction(2, 3) - Fraction(1, 3)) should be === Fraction(1, 3)
    (Fraction(3, 8) - Fraction(1, 2)) should be === Fraction(-1, 8)

    (Fraction(4, 2) / Fraction(1, 2)) should be === Fraction(4, 1)
    (Fraction(3, 5) / Fraction(1, 4)) should be === Fraction(12, 5)
    (Fraction(1, 1) / Fraction(1, 1)) should be === Fraction(1, 1)
  }
}

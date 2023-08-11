import convenience.Applicative
import org.scalatest.FunSuite

class ApplicativeSuite extends FunSuite {
  test("map2 test happy path") {
    val a: Either[Throwable, String] = Right("nice a")
    val b: Either[Throwable, String]  = Right("nice b")
    val added = Applicative.mapN(a, b)((aa, bb) => aa +" and " + bb)

  }
}
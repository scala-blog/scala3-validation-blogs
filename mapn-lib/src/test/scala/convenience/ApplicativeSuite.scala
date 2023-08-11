package convenience

import convenience.Applicative
import convenience.Applicative.EitherError
import org.scalatest.flatspec.AnyFlatSpec

class ApplicativeSuite  extends AnyFlatSpec {
  "map2" should "happy path" in {
    val a: Either[List[Throwable], String] = Right("nice a")
    val b: Either[List[Throwable], String]  = Right("nice b")
    val added: EitherError[String] = Applicative.mapN(a, b)((aa, bb) => aa + " and " + bb)
    added match
      case Left(_) => assert(false, "It should not show errors")
      case Right(o) => assert(o == "nice a and nice b")
  }
  "map2" should "a bad" in {
    val a: Either[List[Throwable], String] = Left(List(Throwable("bad a")))
    val b: Either[List[Throwable], String]  = Right("nice b")
    val added: EitherError[String] = Applicative.mapN(a, b)((aa, bb) => aa + " and " + bb)
    added match
      case Left(o) =>
        assert(o.length == 1)
        assert(o(0).getMessage == "bad a")
      case Right(o) => assert(false, "It should fail")
  }

  "map2" should "a and b bad" in {
    val a: Either[List[Throwable], String] = Left(List(Throwable("bad a")))
    val b: Either[List[Throwable], String]  = Left(List(Throwable("bad b")))
    val added: EitherError[String] = Applicative.mapN(a, b)((aa, bb) => aa + " and " + bb)
    added match
      case Left(o) =>
        assert(o.length == 2)
        assert(o(0).getMessage == "bad a")
        assert(o(1).getMessage == "bad b")
      case Right(o) => assert(false, "It should fail")
  }
}
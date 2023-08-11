package convenience

object Lib {
  type EitherError[A] = Either[List[Throwable], A]
  def LeftThrowable[A](message: String): Either[List[Throwable], A] =
    Left(List(Throwable(message)))

  def SingleError[A, E <: Throwable](throwable: E): Either[List[Throwable], A] =
    Left(List(throwable))
}

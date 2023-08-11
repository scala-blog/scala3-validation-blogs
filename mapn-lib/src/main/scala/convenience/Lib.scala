package convenience

object Lib {
  def LeftThrowable[A](message: String): Either[List[Throwable], A] =
    Left(List(Throwable(message)))

  def SingleError[A, E <: Throwable](throwable: E): Either[List[Throwable], A] =
    Left(List(throwable))
}

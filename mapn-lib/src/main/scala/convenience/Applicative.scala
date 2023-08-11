package convenience


object MapN
  type EitherError[A] = Either[List[Throwable], A]
  def mapN[A1, A2, R](a1Either: EitherError[A1], a2Either:EitherError[A2])(
                  f: (A1, A2) => R
  ): EitherError[R] =
    a1Either match
      case Left(e1) =>
        a2Either match
          case Left(e2) =>
            Left(e1 ++ e2)
          case Right(_) =>
            Left(e1)
      case Right(o1) =>
        a2Either match
          case Left(e2) =>
            Left(e2)
          case Right(o2) =>
            Right(f(o1, o2))


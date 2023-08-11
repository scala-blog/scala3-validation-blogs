package convenience


object Applicative:
  type EitherError[A] = Either[List[Throwable], A]


  def mapN[A1, A2, R](a1Either: => EitherError[A1], a2Either: => EitherError[A2])(
    f: (A1, A2) => R
  ): EitherError[R] =
    val eitherList: Seq[EitherError[Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, R](a1Either: => EitherError[A1], a2Either: => EitherError[A2], a3Either: => EitherError[A3])(
    f: (A1, A2, A3) => R
  ): EitherError[R] =
    val eitherList: Seq[EitherError[Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, R](a1Either: => EitherError[A1], a2Either: => EitherError[A2], a3Either: => EitherError[A3], a4Either: => EitherError[A4])(
    f: (A1, A2, A3, A4) => R
  ): EitherError[R] =
    val eitherList: Seq[EitherError[Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, A5, R](a1Either: => EitherError[A1], a2Either: => EitherError[A2], a3Either: => EitherError[A3], a4Either: => EitherError[A4], a5Either: => EitherError[A5])(
    f: (A1, A2, A3, A4, A5) => R
  ): EitherError[R] =
    val eitherList: Seq[EitherError[Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4], goodParams(4).asInstanceOf[A5]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, A5, A6, R](a1Either: => EitherError[A1], a2Either: => EitherError[A2], a3Either: => EitherError[A3], a4Either: => EitherError[A4], a5Either: => EitherError[A5], a6Either: => EitherError[A6])(
    f: (A1, A2, A3, A4, A5, A6) => R
  ): EitherError[R] =
    val eitherList: Seq[EitherError[Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4], goodParams(4).asInstanceOf[A5], goodParams(5).asInstanceOf[A6]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, A5, A6, A7, R](a1Either: => EitherError[A1], a2Either: => EitherError[A2], a3Either: => EitherError[A3], a4Either: => EitherError[A4], a5Either: => EitherError[A5], a6Either: => EitherError[A6], a7Either: => EitherError[A7])(
    f: (A1, A2, A3, A4, A5, A6, A7) => R
  ): EitherError[R] =
    val eitherList: Seq[EitherError[Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4], goodParams(4).asInstanceOf[A5], goodParams(5).asInstanceOf[A6], goodParams(6).asInstanceOf[A7]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, A5, A6, A7, A8, R](a1Either: => EitherError[A1], a2Either: => EitherError[A2], a3Either: => EitherError[A3], a4Either: => EitherError[A4], a5Either: => EitherError[A5], a6Either: => EitherError[A6], a7Either: => EitherError[A7], a8Either: => EitherError[A8])(
    f: (A1, A2, A3, A4, A5, A6, A7, A8) => R
  ): EitherError[R] =
    val eitherList: Seq[EitherError[Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4], goodParams(4).asInstanceOf[A5], goodParams(5).asInstanceOf[A6], goodParams(6).asInstanceOf[A7], goodParams(7).asInstanceOf[A8]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, A5, A6, A7, A8, A9, R](a1Either: => EitherError[A1], a2Either: => EitherError[A2], a3Either: => EitherError[A3], a4Either: => EitherError[A4], a5Either: => EitherError[A5], a6Either: => EitherError[A6], a7Either: => EitherError[A7], a8Either: => EitherError[A8], a9Either: => EitherError[A9])(
    f: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => R
  ): EitherError[R] =
    val eitherList: Seq[EitherError[Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4], goodParams(4).asInstanceOf[A5], goodParams(5).asInstanceOf[A6], goodParams(6).asInstanceOf[A7], goodParams(7).asInstanceOf[A8], goodParams(8).asInstanceOf[A9]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, R](a1Either: => EitherError[A1], a2Either: => EitherError[A2], a3Either: => EitherError[A3], a4Either: => EitherError[A4], a5Either: => EitherError[A5], a6Either: => EitherError[A6], a7Either: => EitherError[A7], a8Either: => EitherError[A8], a9Either: => EitherError[A9], a10Either: => EitherError[A10])(
    f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => R
  ): EitherError[R] =
    val eitherList: Seq[EitherError[Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4], goodParams(4).asInstanceOf[A5], goodParams(5).asInstanceOf[A6], goodParams(6).asInstanceOf[A7], goodParams(7).asInstanceOf[A8], goodParams(8).asInstanceOf[A9], goodParams(9).asInstanceOf[A10]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, R](a1Either: => EitherError[A1], a2Either: => EitherError[A2], a3Either: => EitherError[A3], a4Either: => EitherError[A4], a5Either: => EitherError[A5], a6Either: => EitherError[A6], a7Either: => EitherError[A7], a8Either: => EitherError[A8], a9Either: => EitherError[A9], a10Either: => EitherError[A10], a11Either: => EitherError[A11])(
    f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => R
  ): EitherError[R] =
    val eitherList: Seq[EitherError[Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4], goodParams(4).asInstanceOf[A5], goodParams(5).asInstanceOf[A6], goodParams(6).asInstanceOf[A7], goodParams(7).asInstanceOf[A8], goodParams(8).asInstanceOf[A9], goodParams(9).asInstanceOf[A10], goodParams(10).asInstanceOf[A11]))
      case _ => Left(errors)


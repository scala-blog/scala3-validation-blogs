package convenience


object Applicative:


  def mapN[A1, A2, R](a1Either: => Either[List[Throwable], A1], a2Either: => Either[List[Throwable], A2])(
    f: (A1, A2) => R
  ): Either[List[Throwable], R] =
    val eitherList: Seq[Either[List[Throwable], Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, R](a1Either: => Either[List[Throwable], A1], a2Either: => Either[List[Throwable], A2], a3Either: => Either[List[Throwable], A3])(
    f: (A1, A2, A3) => R
  ): Either[List[Throwable], R] =
    val eitherList: Seq[Either[List[Throwable], Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, R](a1Either: => Either[List[Throwable], A1], a2Either: => Either[List[Throwable], A2], a3Either: => Either[List[Throwable], A3], a4Either: => Either[List[Throwable], A4])(
    f: (A1, A2, A3, A4) => R
  ): Either[List[Throwable], R] =
    val eitherList: Seq[Either[List[Throwable], Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, A5, R](a1Either: => Either[List[Throwable], A1], a2Either: => Either[List[Throwable], A2], a3Either: => Either[List[Throwable], A3], a4Either: => Either[List[Throwable], A4], a5Either: => Either[List[Throwable], A5])(
    f: (A1, A2, A3, A4, A5) => R
  ): Either[List[Throwable], R] =
    val eitherList: Seq[Either[List[Throwable], Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4], goodParams(4).asInstanceOf[A5]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, A5, A6, R](a1Either: => Either[List[Throwable], A1], a2Either: => Either[List[Throwable], A2], a3Either: => Either[List[Throwable], A3], a4Either: => Either[List[Throwable], A4], a5Either: => Either[List[Throwable], A5], a6Either: => Either[List[Throwable], A6])(
    f: (A1, A2, A3, A4, A5, A6) => R
  ): Either[List[Throwable], R] =
    val eitherList: Seq[Either[List[Throwable], Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4], goodParams(4).asInstanceOf[A5], goodParams(5).asInstanceOf[A6]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, A5, A6, A7, R](a1Either: => Either[List[Throwable], A1], a2Either: => Either[List[Throwable], A2], a3Either: => Either[List[Throwable], A3], a4Either: => Either[List[Throwable], A4], a5Either: => Either[List[Throwable], A5], a6Either: => Either[List[Throwable], A6], a7Either: => Either[List[Throwable], A7])(
    f: (A1, A2, A3, A4, A5, A6, A7) => R
  ): Either[List[Throwable], R] =
    val eitherList: Seq[Either[List[Throwable], Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4], goodParams(4).asInstanceOf[A5], goodParams(5).asInstanceOf[A6], goodParams(6).asInstanceOf[A7]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, A5, A6, A7, A8, R](a1Either: => Either[List[Throwable], A1], a2Either: => Either[List[Throwable], A2], a3Either: => Either[List[Throwable], A3], a4Either: => Either[List[Throwable], A4], a5Either: => Either[List[Throwable], A5], a6Either: => Either[List[Throwable], A6], a7Either: => Either[List[Throwable], A7], a8Either: => Either[List[Throwable], A8])(
    f: (A1, A2, A3, A4, A5, A6, A7, A8) => R
  ): Either[List[Throwable], R] =
    val eitherList: Seq[Either[List[Throwable], Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4], goodParams(4).asInstanceOf[A5], goodParams(5).asInstanceOf[A6], goodParams(6).asInstanceOf[A7], goodParams(7).asInstanceOf[A8]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, A5, A6, A7, A8, A9, R](a1Either: => Either[List[Throwable], A1], a2Either: => Either[List[Throwable], A2], a3Either: => Either[List[Throwable], A3], a4Either: => Either[List[Throwable], A4], a5Either: => Either[List[Throwable], A5], a6Either: => Either[List[Throwable], A6], a7Either: => Either[List[Throwable], A7], a8Either: => Either[List[Throwable], A8], a9Either: => Either[List[Throwable], A9])(
    f: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => R
  ): Either[List[Throwable], R] =
    val eitherList: Seq[Either[List[Throwable], Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4], goodParams(4).asInstanceOf[A5], goodParams(5).asInstanceOf[A6], goodParams(6).asInstanceOf[A7], goodParams(7).asInstanceOf[A8], goodParams(8).asInstanceOf[A9]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, R](a1Either: => Either[List[Throwable], A1], a2Either: => Either[List[Throwable], A2], a3Either: => Either[List[Throwable], A3], a4Either: => Either[List[Throwable], A4], a5Either: => Either[List[Throwable], A5], a6Either: => Either[List[Throwable], A6], a7Either: => Either[List[Throwable], A7], a8Either: => Either[List[Throwable], A8], a9Either: => Either[List[Throwable], A9], a10Either: => Either[List[Throwable], A10])(
    f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => R
  ): Either[List[Throwable], R] =
    val eitherList: Seq[Either[List[Throwable], Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4], goodParams(4).asInstanceOf[A5], goodParams(5).asInstanceOf[A6], goodParams(6).asInstanceOf[A7], goodParams(7).asInstanceOf[A8], goodParams(8).asInstanceOf[A9], goodParams(9).asInstanceOf[A10]))
      case _ => Left(errors)



  def mapN[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, R](a1Either: => Either[List[Throwable], A1], a2Either: => Either[List[Throwable], A2], a3Either: => Either[List[Throwable], A3], a4Either: => Either[List[Throwable], A4], a5Either: => Either[List[Throwable], A5], a6Either: => Either[List[Throwable], A6], a7Either: => Either[List[Throwable], A7], a8Either: => Either[List[Throwable], A8], a9Either: => Either[List[Throwable], A9], a10Either: => Either[List[Throwable], A10], a11Either: => Either[List[Throwable], A11])(
    f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => R
  ): Either[List[Throwable], R] =
    val eitherList: Seq[Either[List[Throwable], Any]] = List(a1Either, a2Either)
    val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
    val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
    errors match
      case Nil => Right(f(goodParams(0).asInstanceOf[A1], goodParams(1).asInstanceOf[A2], goodParams(2).asInstanceOf[A3], goodParams(3).asInstanceOf[A4], goodParams(4).asInstanceOf[A5], goodParams(5).asInstanceOf[A6], goodParams(6).asInstanceOf[A7], goodParams(7).asInstanceOf[A8], goodParams(8).asInstanceOf[A9], goodParams(9).asInstanceOf[A10], goodParams(10).asInstanceOf[A11]))
      case _ => Left(errors)

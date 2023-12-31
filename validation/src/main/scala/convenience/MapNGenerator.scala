package convenience

object MapNGenerator extends App:
  val n = 10

  println("""package convenience
            |
            |
            |object Applicative:
            |  """.stripMargin)
  for(k <- 1 to n) {
    val i = k+1
    val A_List = (1 to i).map{j => s"A${j}"}.mkString(", ")
    val methodParams: String = (1 to i).map{ j =>
      s"a${j}Either: => Either[List[Throwable], A$j]"
    }.mkString(", ")
    val fParams: String = (1 to i).map { j =>
      s"goodParams(${j-1}).asInstanceOf[A$j]"
    }.mkString(", ")
    println(
      s"""
         |  def mapN[$A_List, R]($methodParams)(
         |    f: ($A_List) => R
         |    ): Either[List[Throwable], R] =
         |      val eitherList: Seq[Either[List[Throwable], Any]] = List(a1Either, a2Either)
         |      val errors: List[Throwable] = eitherList.collect{case Left(e) => e}.flatten.toList
         |      val goodParams: Seq[Any] = eitherList.collect{case Right(o) => o}.toIndexedSeq
         |      errors match
         |        case Nil => Right(f($fParams))
         |        case _ => Left(errors)
         |
         |""".stripMargin)
  }

package datavalidation


object UnionTypeVsEither extends App:
  final case class Email(user: String, domain: String)
  final case class SSN(area: Int, group: Int, serial: Int)
  final case class Employee(email: Email, ssn: SSN)

  object UsingNoValidationWrapper:
    object SsnBuilder:
      def fromString(string: String): SSN = {
        string match
          case null =>
            throw new Throwable("Social security is null")
          case _ =>
            val split = string.split("-")
            if (split.size != 3)
              throw new Throwable(s"Three different sets of digits expected but ${split.size} found")
            else if (split(0).filter(_.isDigit).isEmpty)
              throw new Throwable(s"No digits found in area position '${string}'")
            else if (split(1).filter(_.isDigit).isEmpty)
              throw new Throwable(s"No digits found in group position '${string}'")
            else if (split(2).filter(_.isDigit).isEmpty)
              throw new Throwable(s"No digits found in serial position '${string}'")
            else if (split(0).filter(!_.isDigit).nonEmpty)
              throw new Throwable(s"Invalid digit found in area  position '${string}'")
            else if (split(1).filter(!_.isDigit).nonEmpty)
              throw new Throwable(s"Invalid digit found in group position '${string}'")
            else if (split(2).filter(!_.isDigit).nonEmpty)
              throw new Throwable(s"Invalid digit found in serial position '${string}'")
            else
              SSN(area = split(0).toInt, group = split(1).toInt, serial = split(2).toInt)
      }

    object EmailBuilder:
      def fromString(string: String): Email = string match
        case null =>
          throw new Throwable("Email is null")
        case _ =>
          val split = string.split("@")
          if (split.size != 2)
            throw new Throwable(s"Email '${string}' is malformed")
          else
            Email(user = split(0), domain = split(1))


  object UsingUnionType:
    type unionWithErrorList[A] = List[Throwable] | A

    object SsnBuilder:
      def fromString(string: String): unionWithErrorList[SSN] = {
        string match
          case null =>
            List(Throwable("Social security is null"))
          case _ =>
            val split = string.split("-")
            if (split.size != 3)
              List(Throwable(s"Three different sets of digits expected but ${split.size} found"))
            else if (split(0).filter(_.isDigit).isEmpty)
              List(Throwable(s"No digits found in area position '${string}'"))
            else if (split(1).filter(_.isDigit).isEmpty)
              List(Throwable(s"No digits found in group position '${string}'"))
            else if (split(2).filter(_.isDigit).isEmpty)
              List(Throwable(s"No digits found in serial position '${string}'"))
            else if (split(0).filter(!_.isDigit).nonEmpty)
              List(Throwable(s"Invalid digit found in area  position '${string}'"))
            else if (split(1).filter(!_.isDigit).nonEmpty)
              List(Throwable(s"Invalid digit found in group position '${string}'"))
            else if (split(2).filter(!_.isDigit).nonEmpty)
              List(Throwable(s"Invalid digit found in serial position '${string}'"))
            else
              SSN(area = split(0).toInt, group = split(1).toInt, serial = split(2).toInt)
      }

    object EmailBuilder:
      def fromString(string: String):  unionWithErrorList[Email] = string match
        case null =>
          List(Throwable("Email is null"))
        case _ =>
          val split = string.split("@")
          if (split.size != 2)
            List(Throwable(s"Email '${string}' is malformed"))
          else
            Email(user = split(0), domain = split(1))




  object UsingEither:
    object SsnBuilder:
      def fromString(string: String): Either[List[Throwable], SSN] = {
        string match
          case null =>
            Left(List(Throwable("Social security is null")))
          case _ =>
            val split = string.split("-")
            if (split.size != 3)
              Left(List(Throwable(s"Three different sets of digits expected but ${split.size} found")))
            else if (split(0).filter(_.isDigit).isEmpty)
              Left(List(Throwable(s"No digits found in area position '${string}'")))
            else if (split(1).filter(_.isDigit).isEmpty)
              Left(List(Throwable(s"No digits found in group position '${string}'")))
            else if (split(2).filter(_.isDigit).isEmpty)
              Left(List(Throwable(s"No digits found in serial position '${string}'")))
            else if (split(0).filter(!_.isDigit).nonEmpty)
              Left(List(Throwable(s"Invalid digit found in area  position '${string}'")))
            else if (split(1).filter(!_.isDigit).nonEmpty)
              Left(List(Throwable(s"Invalid digit found in group position '${string}'")))
            else if (split(2).filter(!_.isDigit).nonEmpty)
              Left(List(Throwable(s"Invalid digit found in serial position '${string}'")))
            else
              Right(SSN(area = split(0).toInt, group = split(1).toInt, serial = split(2).toInt))
      }


    object EmailBuilder:
      def fromString(string: String): Either[List[Throwable], Email] = string match
        case null =>
          Left(List(Throwable("Email is null")))
        case _ =>
          val split = string.split("@")
          if (split.size != 2)
            Left(List(Throwable(s"Email '${string}' is malformed")))
          else
            Right(Email(user = split(0), domain = split(1)))


  extension[B] (or: UsingUnionType.unionWithErrorList[B])
    def flatMap[B1](f: B => List[Throwable] | B1): UsingUnionType.unionWithErrorList[B1] = or match
      case e: List[Throwable] => e
      case o: B => f(o)

    def map[B1](f: B => B1): UsingUnionType.unionWithErrorList[B1] =
      or match
        case e: List[Throwable] => or.asInstanceOf[UsingUnionType.unionWithErrorList[B1]]
        case o: B => f(o).asInstanceOf[UsingUnionType.unionWithErrorList[B1]]

  val employeeGood = for
    email <- UsingUnionType.EmailBuilder.fromString("x@dd.com")
    ssn <- UsingUnionType.SsnBuilder.fromString("111-11-1111")
  yield Employee(email=email, ssn=ssn)
  println(s"employeeGood $employeeGood")

  val employeeBadSsn = for
    email <- UsingUnionType.EmailBuilder.fromString("x@dd.com")
    ssn <- UsingUnionType.SsnBuilder.fromString("11111")
  yield Employee(email = email, ssn = ssn)
  println(s"employeeBadSsn $employeeBadSsn")

  val employeeBadEmail: UsingUnionType.unionWithErrorList[Employee] = for
    email <- UsingUnionType.EmailBuilder.fromString("x#dd.com")
    ssn <- UsingUnionType.SsnBuilder.fromString("111-11-1111")
  yield Employee(email = email, ssn = ssn)
  println(s"employeeBadEmail $employeeBadEmail")

  val employeeBadEmailAndSsn: UsingUnionType.unionWithErrorList[Employee] = for
    email <- UsingUnionType.EmailBuilder.fromString("x#dd.com")
    ssn <- UsingUnionType.SsnBuilder.fromString("11111")
  yield Employee(email = email, ssn = ssn)
  println(s"employeeBadEmailAndSsn $employeeBadEmailAndSsn")

  def employeeGenerateValidateWithUnionTypes(size: Int): List[UsingUnionType.unionWithErrorList[Employee]] = {
    val employees: List[UsingUnionType.unionWithErrorList[Employee]] = {0 until size}.map { i =>
      val last4 = i % 8000 + 1000
      val first3 = i % 830 + 101
      val second2 = i % 81 + 12
      val employee = for {
        s <- UsingUnionType.SsnBuilder.fromString(first3+"-"+second2 + "-" + last4)
        e <- UsingUnionType.EmailBuilder.fromString( "email"+i++"@mail.com")
        empl <- Employee(ssn = s, email =e)
      } yield empl
      employee
    }.toList
    employees
  }

  def employeeGenerateValidateWithEither(size: Int): List[Either[List[Throwable], Employee]] = {
    val employees: List[Either[List[Throwable],  Employee]] = {
      0 until size
    }.map { i =>
      val last4 = i % 8000 + 1000
      val first3 = i % 830 + 101
      val second2 = i % 81 + 12
      val employee = for {
        s <- UsingEither.SsnBuilder.fromString(first3 + "-" + second2 + "-" + last4)
        e <- UsingEither.EmailBuilder.fromString("email" + i ++ "@mail.com")
        empl = Employee(ssn = s, email = e)
      } yield empl
      employee
    }.toList
    employees
  }

  def employeeGenerateNoValidationWrappers(size: Int): List[Employee] = {
    val employees: List[Employee] = {
      0 until size
    }.map { i =>
      val last4 = i % 8000 + 1000
      val first3 = i % 830 + 101
      val second2 = i % 81 + 12

      val s = UsingUnionType.SsnBuilder.fromString(first3 + "-" + second2 + "-" + last4)
      val e = UsingUnionType.EmailBuilder.fromString("email" + i ++ "@mail.com")

      Employee(ssn = s.asInstanceOf[SSN], email = e.asInstanceOf[Email])
    }.toList
    employees
  }

object BenchmarkEither extends App:
  UnionTypeVsEither.employeeGenerateValidateWithEither(4_000_000)

  val t1 = System.currentTimeMillis()
  val m1 = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()

  val list = UnionTypeVsEither.employeeGenerateValidateWithEither(4_000_000)
  val t2 = System.currentTimeMillis()
  val m2 = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()

  println(s"employeeGenerateValidateWithEither:mem:gb: ${(m2 - m1) / 1_000_000_000.0}")
  println(s"employeeGenerateValidateWithEither:ms: ${(t2 - t1)}")

object BenchmarkUnionType extends App:
  UnionTypeVsEither.employeeGenerateValidateWithUnionTypes(4_000_000)

  val t1 = System.currentTimeMillis()
  val m1 = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()

  val list = UnionTypeVsEither.employeeGenerateValidateWithUnionTypes(4_000_000)
  val t2 = System.currentTimeMillis()
  val m2 = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()

  println(s"employeeGenerateValidateWithUnionTypes:mem:gb: ${(m2-m1)/1_000_000_000.0}")
  println(s"employeeGenerateValidateWithUnionTypes:ms: ${(t2 - t1)}")

object NoWrappers extends App:
  UnionTypeVsEither.employeeGenerateNoValidationWrappers(4_000_000)

  val t1 = System.currentTimeMillis()
  val m1 = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()

  val list = UnionTypeVsEither.employeeGenerateNoValidationWrappers(4_000_000)
  val t2 = System.currentTimeMillis()
  val m2 = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()

  println(f"employeeGenerateNoValidationWrappers:mem:gb: ${(m2 - m1) / 1_000_000_000.0}%.2f")
  println(s"employeeGenerateNoValidationWrappers:ms: ${(t2 - t1)}")
  /*
  -XX:+UseEpsilonGC
  union
  3.405825744 GB
  final: employeeGenerateValidateWithUnionTypes:ms: 2353
  3.405825744 GB
  final: employeeGenerateValidateWithUnionTypes:ms: 2385

  either
  3.63231816 GB
  final: employeeGenerateValidateWithEither:ms: 2430
  3.636512464 GB
  final: employeeGenerateValidateWithEither:ms: 2496

  nowrappers
  0.918994944 GB
  final: employeeGenerateValidateWithUnionTypes:ms: 3005
   */
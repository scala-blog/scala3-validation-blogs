package datavalidation

package datavalidation

import convenience.ApplicativeMain.{Email3, Employee3, SSN3}


object UnionTypeVsEitherOld extends App:
  type unionWithErrorList[A] = List[Throwable] | A
  final case class SSN4 private(area: Int, group: Int, serial: Int)

  object SSN4:
    def fromString(string: String): unionWithErrorList[SSN4] = {
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
            SSN4(area = split(0).toInt, group = split(1).toInt, serial = split(2).toInt)
    }

  final case class Email4 private(user: String, domain: String)

  object Email4:
    def fromString(string: String):  unionWithErrorList[Email4] = string match
      case null =>
        List(Throwable("Email is null"))
      case _ =>
        val split = string.split("@")
        if (split.size != 2)
          List(Throwable(s"Email '${string}' is malformed"))
        else
          Email4(user = split(0), domain = split(1))

  final case class Employee4(ssn: SSN4, email: Email4)

  /*
  def map[B1](f: B => B1): Either[A, B1] = this match {
    case Right(b) => Right(f(b))
    case _        => this.asInstanceOf[Either[A, B1]]
  }
   */
  extension[B] (or: unionWithErrorList[B])
    def flatMap[B1](f: B => List[Throwable] | B1): unionWithErrorList[B1] = or match
      case e: List[Throwable] => e
      case o: B => f(o)

    def map[B1](f: B => B1): unionWithErrorList[B1] =
      or match
        case e: List[Throwable] => or.asInstanceOf[unionWithErrorList[B1]]
        case o: B => f(o).asInstanceOf[unionWithErrorList[B1]]



  val employeeGood = for
    email <- Email4.fromString("x@dd.com")
    ssn <- SSN4.fromString("111-11-1111")
  yield Employee4(email=email, ssn=ssn)
  println(employeeGood)

  val employeeBadSsn = for
    email <- Email4.fromString("x@dd.com")
    ssn <- SSN4.fromString("11111")
  yield Employee4(email = email, ssn = ssn)
  println(employeeBadSsn)

  val employeeBadEmail: unionWithErrorList[Employee4] = for
    email <- Email4.fromString("x#dd.com")
    ssn <- SSN4.fromString("111-11-1111")
  yield Employee4(email = email, ssn = ssn)
  println(employeeBadEmail)

  val employeeBadEmailAndSsn: unionWithErrorList[Employee4] = for
    email <- Email4.fromString("x#dd.com")
    ssn <- SSN4.fromString("11111")
  yield Employee4(email = email, ssn = ssn)
  println(employeeBadEmailAndSsn)

  def employeeGenerator(): List[unionWithErrorList[Employee4]] = {
    val employees: List[unionWithErrorList[Employee4]] = {0 until 1000000}.map { i =>
      val last4 = i % 8000 + 1000
      val first3 = i % 830 + 101
      val second2 = i % 81 + 12
      val employee = for {
        s <- SSN4.fromString(first3+"-"+second2 + "-" + last4)
        e <- Email4.fromString( "email"+i++"@mail.com")
        empl <- Employee4(ssn = s, email =e)
      } yield empl
      employee
    }.toList
    //println(employees)
    employees
  }

  def employeeGenerator2(): List[Either[List[Throwable], Employee3]] = {
    val employees: List[Either[List[Throwable],  Employee3]] = {
      0 until 1000000
    }.map { i =>
      val last4 = i % 8000 + 1000
      val first3 = i % 830 + 101
      val second2 = i % 81 + 12
      val employee = for {
        s <- SSN3.fromString(first3 + "-" + second2 + "-" + last4)
        e <- Email3.fromString("email" + i ++ "@mail.com")
        empl = Employee3(ssn = s, email = e)
      } yield empl
      employee
    }.toList
    //println(employees)
    employees
  }

  {
    val s1 = System.currentTimeMillis()
    employeeGenerator()
    val s2 = System.currentTimeMillis()

    employeeGenerator2()
    val s3 = System.currentTimeMillis()
    println("speed1: " + (s2 - s1).toString)
    println("speed12 " + (s3 - s2).toString)
  }
  {
    val s1 = System.currentTimeMillis()
    employeeGenerator()
    val s2 = System.currentTimeMillis()

    employeeGenerator2()
    val s3 = System.currentTimeMillis()
    println("speed1: " + (s2 - s1).toString)
    println("speed12 " + (s3 - s2).toString)
  }
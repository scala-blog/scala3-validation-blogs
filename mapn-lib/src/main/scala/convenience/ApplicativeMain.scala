package convenience

import convenience.Lib.{EitherError, LeftThrowable}


object ApplicativeMain extends App:

  final case class SSN2 private(area: Int, group: Int, serial: Int)
  object SSN2:
    def fromString(string: String): Either[List[Throwable], SSN2] = string match
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
          Right(SSN2(area = split(0).toInt, group = split(1).toInt, serial = split(2).toInt))

  final case class Email2 private(user: String, domain: String)
  object Email2:
    def fromString(string: String): Either[List[Throwable], Email2] = string match
      case null =>
        Left(List(Throwable("Email is null")))
      case _ =>
        val split = string.split("@")
        if (split.size != 2)
          Left(List(Throwable(s"Email '${string}' is malformed")))
        else
          Right(Email2(user = split(0), domain = split(1)))


  final case class SSN3 private(area: Int, group: Int, serial: Int)
  object SSN3:
    def fromString(string: String): Either[List[Throwable], SSN3] = string match
      case null =>
        LeftThrowable("Social security is null")
      case _ =>
        val split = string.split("-")
        if (split.size != 3)
          LeftThrowable(s"Three different sets of digits expected but ${split.size} found")
        else if (split(0).filter(_.isDigit).isEmpty)
          LeftThrowable(s"No digits found in area position '${string}'")
        else if (split(1).filter(_.isDigit).isEmpty)
          LeftThrowable(s"No digits found in group position '${string}'")
        else if (split(2).filter(_.isDigit).isEmpty)
          LeftThrowable(s"No digits found in serial position '${string}'")
        else if (split(0).filter(!_.isDigit).nonEmpty)
          LeftThrowable(s"Invalid digit found in area  position '${string}'")
        else if (split(1).filter(!_.isDigit).nonEmpty)
          LeftThrowable(s"Invalid digit found in group position '${string}'")
        else if (split(2).filter(!_.isDigit).nonEmpty)
          LeftThrowable(s"Invalid digit found in serial position '${string}'")
        else
          Right(SSN3(area = split(0).toInt, group = split(1).toInt, serial = split(2).toInt))

  final case class Email3 private(user: String, domain: String)
  object Email3:
    def fromString(string: String): Either[List[Throwable], Email3] = string match
      case null =>
        LeftThrowable("Email is null")
      case _ =>
        val split = string.split("@")
        if (split.size != 2)
          LeftThrowable(s"Email '${string}' is malformed")
        else
          Right(Email3(user = split(0), domain = split(1)))

  final case class Employee(ssn: SSN3, email: Email3)

  val employeeGood: EitherError[Employee] = Applicative.mapN(
    Email3.fromString("hernan@gmail.com"),
    SSN3.fromString("111-11-1111")
  )((email, ssn) => Employee(email = email, ssn = ssn))
  println(employeeGood)
  // Right(Employee(SSN2(111,11,1111),Email2(hernan,gmail.com)))

  val employeeBadEmail: EitherError[Employee] = Applicative.mapN(
    Email3.fromString("hernan#gmail.com"),
    SSN3.fromString("111-11-1111")
  )((email, ssn) => Employee(email = email, ssn = ssn))
  println(employeeBadEmail)
  // Left(List(java.lang.Throwable: Email 'hernan#gmail.com' is malformed))

  val employeeBadEmailAndSsn: EitherError[Employee] = Applicative.mapN(
    Email3.fromString("hernan#gmail.com"),
    SSN3.fromString("111111111")
  )((email, ssn) => Employee(email = email, ssn = ssn))
  println(employeeBadEmailAndSsn)
  // Left(List(java.lang.Throwable: Email 'hernan#gmail.com' is malformed, java.lang.Throwable: Three different sets of digits expected but 1 found))
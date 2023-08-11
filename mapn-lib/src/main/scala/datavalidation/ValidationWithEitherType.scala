package datavalidation

import scala.util.{Failure, Try}

object ValidationWithEitherType extends App :

  def validateEmail(string: String): Unit =
    if (string == null)
      throw new Throwable("Email is null")
    else
      val split = string.split("@")
      if (split.size != 2)
        throw new Throwable(s"Email ${string} is malformed")
      else ()

  def validateEmailEither(string: String): Either[Throwable, Unit] = string match
    case null =>
      Left(Throwable("Email is null"))
    case _ =>
      val split = string.split("@")
      if (split.size != 2)
        Left(Throwable(s"Email ${string} is malformed"))
      else Right(())


  // validateEmail("hernan#email.com")
  // Exception in thread "main" java.lang.Throwable: Email hernan#email.com is malformed

  // The following throws no exception
  val validated: Either[Throwable, Unit] = validateEmailEither("hernan#email.com")
  // We can then selectively handle the error when it occurs
  validated match
    case Left(f) => println(s"validation fail: ${f.getMessage}")


  case class SSN private(area: Int, group: Int, serial: Int)
  object SSN:
    def fromString(string: String): Either[Throwable, SSN] = string match
      case null =>
        Left(Throwable("Social security is null"))
      case _ =>
        val split = string.split("-")
        if (split.size != 3)
          Left(Throwable(s"Three different sets of digits expected but ${split.size} found"))
        else if (split(0).filter(_.isDigit).size == 0)
          Left(Throwable(s"No digits found in area position '${string}'"))
        else if (split(1).filter(_.isDigit).size == 0)
          Left(Throwable(s"No digits found in group position '${string}'"))
        else if (split(2).filter(_.isDigit).size == 0)
          Left(Throwable(s"No digits found in serial position '${string}'"))
        else if (split(0).filter(!_.isDigit).size != 0)
          Left(Throwable(s"Invalid digit found in area  position '${string}'"))
        else if (split(1).filter(!_.isDigit).size != 0)
          Left(Throwable(s"Invalid digit found in group position '${string}'"))
        else if (split(2).filter(!_.isDigit).size != 0)
          Left(Throwable(s"Invalid digit found in serial position '${string}'"))
        else
          Right(SSN(area = split(0).toInt, group = split(1).toInt, serial = split(2).toInt))

  case class Email private(user: String, domain: String)
  object Email:
    def fromString(string: String): Either[Throwable, Email] = string match
      case null =>
        Left(Throwable("Email is null"))
      case _ =>
        val split = string.split("@")
        if (split.size != 2)
          Left(Throwable(s"Email '${string}' is malformed"))
        else
          Right(Email(user = split(0), domain = split(1)))


  final case class Employee(ssn: SSN, email: Email)

  val employee: Either[Throwable, Employee] = for
    email <- Email.fromString("hernan@email.com")
    ssn <- SSN.fromString("111-11-1111")
  yield Employee(ssn = ssn, email = email)

  val employeeBadEmail: Either[Throwable, Employee] = for
    email <- Email.fromString("hernan#email.com")
    ssn <- SSN.fromString("111-11-1111")
  yield Employee(ssn = ssn, email = email)

  val employeeBadSsn: Either[Throwable, Employee] = for
    email <- Email.fromString("hernan@email.com")
    ssn <- SSN.fromString("11111-1111")
  yield Employee(ssn = ssn, email = email)

  // Again, we handle errors using normal control data flow
  employee match {
    case Right(o) => println(s"employee: Validated employee: $o")
    case Left(e) => println(s"employee: Validation error: $e")
  }
  // employee: Validated employee: Employee(SSN(111,11,1111),Email(hernan,email.com))

  employeeBadEmail match {
    case Right(o) => println(s"employeeBadEmail: Validated employee: $o")
    case Left(e) => println(s"employeeBadEmail: Validation error: $e")
  }
  // employeeBadEmail: Validation error: java.lang.Throwable: Email 'hernan#email.com' is malformed

  employeeBadSsn match {
    case Right(o) => println(s"employeeBadSsn: Validated employee: $o")
    case Left(e) => println(s"employeeBadSsn: Validation error: $e")
  }
  // employeeBadSsn: Validation error: java.lang.Throwable: Three different sets of digits expected but 2 found


  // We have 2 bad fields but we are only evaluation 1.
  // For comprehensions are not equipped to handle multiple validations of fields.
  // First failure will short circuit and cancel next operations
  val employeeBadEmailAndBadSsn: Either[Throwable, Employee] = for
    email <- Email.fromString("hernan#email.com")
    ssn <- SSN.fromString("11111-1111")
  yield Employee(ssn = ssn, email = email)

  employeeBadEmailAndBadSsn match {
    case Right(o) => println(s"employeeBadEmailAndBadSsn: Validated employee: $o")
    case Left(e) => println(s"employeeBadEmailAndBadSsn: Validation error: $e")
  }
  // employeeBadEmailAndBadSsn: Validation error: java.lang.Throwable: Email 'hernan#email.com' is malformed

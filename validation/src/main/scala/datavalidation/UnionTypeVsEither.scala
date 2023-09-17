import convenience.Lib.LeftThrowable

object UnionTypeVsEither extends App:

  final case class SSN4 private(area: Int, group: Int, serial: Int)

  object SSN4:
    def fromString(string: String): List[Throwable] | SSN4 = string match
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

  final case class Email4 private(user: String, domain: String)

  object Email4:
    def fromString(string: String): List[Throwable] | Email4 = string match
      case null =>
        List(Throwable("Email is null"))
      case _ =>
        val split = string.split("@")
        if (split.size != 2)
          List(Throwable(s"Email '${string}' is malformed"))
        else
          Email4(user = split(0), domain = split(1))

  final case class Employee4(ssn: SSN4, email: Email4)


  type unionWithErrorList[A] = List[Throwable] | A
  extension[B](or: unionWithErrorList[B])
    def flatMap[B1](f: B => List[Throwable] | B1): List[Throwable] | B1 = this match {
      case v: unionWithErrorList[B] => v match {
        case e: List[Throwable] => this.asInstanceOf[unionWithErrorList[B1]]
        case o: B => f(o)
      }
  }

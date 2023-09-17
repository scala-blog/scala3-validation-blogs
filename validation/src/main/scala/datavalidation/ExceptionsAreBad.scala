package datavalidation

object ExceptionsAreBad:

  def validateEmail(string: String): Unit =
    if (string == null)
      throw new Error("Email is null")
    else
      val split = string.split("@")
      if (split.size != 2)
        throw new Error(s"Email ${string} is malformed")
      else ()

  case class Email(user: String, domain: String)

  def produceEmail(email: String): Email =
    validateEmail(email)
    // But we already did the same split in validateEmail(email)!
    val splitEmail: Array[String] = email.split("@")
    val emailUser = splitEmail(0)
    val emailDomain = splitEmail(1)
    Email(emailUser, emailDomain)


  // Error channel skips a method
  def sendGreetingEmailErrorSkipMethod(emailString: String): Unit =
    val email: Email = try {
      produceEmail(emailString)
    } catch  {
      case e: Error => println(s"Unable to send email to invalid recipient $emailString")
        return ()
    }
    println(s"sending greeting to $emailString: $email")
    // placeholder
    // code that sends greeting email to email
    //

  def sendGreetingEmailSkipAllMethods(emailString: String): Unit =
    val email: Email = produceEmail(emailString)
    println(s"sending greeting to $emailString: $email")
    // placeholder
    // code that sends greeting email to email
    //
package caseclasses

object CaseClassAccessScopeAndValidation extends App :
  final case class EmployeeValidated(firstName: String, lastName: String, ssn: String):
    require(firstName.nonEmpty)
    require(lastName.nonEmpty)
    require(ssn.nonEmpty)

  // Runs ok
  val employee1: EmployeeValidated = EmployeeValidated("John", "Wick", "111-11-1111")

  // Runtime exception thrown due to "require(lastName.nonEmpty)" validation above
  //val employee2: EmployeeValidated = EmployeeValidated("Michael", "", "222-22-2222")


  final case class Employee(firstName: String, lastName: String, ssn: String)

  object Employee:
    val ssnToFullName: Map[String, (String, String)] = Map(
      "111-11-1111" -> ("John", "Wick"),
      "222-22-2222" -> ("Michael", "Bubble")
    )
    val fullNameToSsn: Map[(String, String), String] = ssnToFullName.map { case (k, v) => (v, k) }

    def fromAllFields(firstName: String, lastName: String, ssn: String): Employee =
      require(firstName.nonEmpty)
      require(lastName.nonEmpty)
      require(ssn.nonEmpty)
      Employee(firstName, lastName, ssn)

    def fromSsn(ssn: String): Employee =
      require(ssn.nonEmpty)
      val (firstName: String, lastName: String) = ssnToFullName(ssn)
      Employee(firstName, lastName, ssn)

    def fromFullName(firstName: String, lastName: String): Employee =
      require(firstName.nonEmpty)
      require(lastName.nonEmpty)
      val ssn: String = fullNameToSsn((firstName, lastName))
      Employee(firstName, lastName, ssn)

  // The following 3 println() should print the same data
  val employee1FromBuilder = Employee.fromSsn("111-11-1111")
  println(employee1FromBuilder)
  val employee2FromBuilder = Employee.fromAllFields("John", "Wick", "111-11-1111")
  println(employee2FromBuilder)
  val employee3FromBuilder = Employee.fromFullName("John", "Wick")
  println(employee3FromBuilder)

  final case class EmployeePrivate private(firstName: String, lastName: String, ssn: String)

  object EmployeePrivate:
    val ssnToFullName: Map[String, (String, String)] = Map(
      "111-11-1111" -> ("John", "Wick"),
      "222-22-2222" -> ("Michael", "Bubble")
    )
    val fullNameToSsn: Map[(String, String), String] = ssnToFullName.map { case (k, v) => (v, k) }

    def fromAllFields(firstName: String, lastName: String, ssn: String): EmployeePrivate =
      require(firstName.nonEmpty)
      require(lastName.nonEmpty)
      require(ssn.nonEmpty)
      EmployeePrivate(firstName, lastName, ssn)

    def fromSsn(ssn: String): EmployeePrivate =
      require(ssn.nonEmpty)
      val (firstName: String, lastName: String) = ssnToFullName(ssn)
      EmployeePrivate(firstName, lastName, ssn)

    def fromFullName(firstName: String, lastName: String): Employee =
      require(firstName.nonEmpty)
      require(lastName.nonEmpty)
      val ssn: String = fullNameToSsn((firstName, lastName))
      Employee(firstName, lastName, ssn)

  // The following 3 println() should print the same data
  val employeePrivate1FromBuilder = EmployeePrivate.fromSsn("111-11-1111")
  println(employee1FromBuilder)
  val employeePrivate2FromBuilder = EmployeePrivate.fromAllFields("John", "Wick", "111-11-1111")
  println(employee2FromBuilder)
  val employeePrivate3FromBuilder = EmployeePrivate.fromFullName("John", "Wick")
  println(employeePrivate3FromBuilder)


  case class NonFinalEmployee(firstName: String, lastName: String, ssn: String)

  // Method 1 for adding derived fields
  // Use a trait and extend Employee case class with trait containing the derived field
  trait FullNameDerived:
    self: NonFinalEmployee =>
    def fullNameDerived1 =
      s"$firstName $lastName"

  class EmployeeData(firstName: String, lastName: String, ssn: String)
    extends NonFinalEmployee(firstName, lastName, ssn) with FullNameDerived

  val employeeData: EmployeeData = new EmployeeData("John", "Wick", "111-11-1111")
  println(employeeData.fullNameDerived1)

  // Method 2 for adding derived fields
  // Use extension method and extend Employee with derived field instead
  extension (c: Employee)
    def fullNameDerived2: String =
      s"${c.firstName} ${c.lastName}"

  val employee = Employee("John", "Wick", "111-11-1111")
  println(employee.fullNameDerived2)

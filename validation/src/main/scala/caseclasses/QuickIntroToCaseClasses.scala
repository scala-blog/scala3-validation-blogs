package caseclasses

object QuickIntroToCaseClasses extends App :
  final case class Employee(firstName: String, lastName: String, ssn: String)

  class EmployeeClass(firstName: String, lastName: String, ssn: String)

  val employee = Employee("john", "wick", "111–11–1111")

  val employeeClass = new EmployeeClass("john", "wick", "111–11–1111")

  println(employee)
  println(employeeClass)


  val employee1 = Employee("john", "wick", "111–11–1111")
  val employee2 = Employee("john", "wick", "111–11–1111")
  val employee3 = Employee("robert", "mccall", "222–222–2222")
  println(employee1 == employee2)
  // true
  println(employee1 == employee3)
  // false

  val employeeClass1 = new EmployeeClass("john", "wick", "111–11–1111")
  val employeeClass2 = new EmployeeClass("john", "wick", "111–11–1111")
  println(employeeClass1 == employeeClass2)

  val employeeWithModifiedLastName = employee1.copy(firstName = "Jon")
  println(employeeWithModifiedLastName)
  // Employee(Jon,wick,111–11–1111)
  println(employee1)
  // FYI, employee1 is not changed
  // Employee(john,wick,111–11–1111)

  final case class MutableEmployee(var firstName: String, var lastName: String, var ssn: String)

  val mutableEmployee = MutableEmployee("gravik", "skrull", "333–33–3333")
  println(mutableEmployee)
  mutableEmployee.firstName = "talos"
  println(mutableEmployee)


  val x: RuntimeException = ???
  employee1 match {
    case Employee(firstName, lastName, ssn) => println(s"Name is $firstName $lastName and ssn is $ssn")
    // Name is john wick and ssn is 111–11–1111
  }

  // No need to define unused fields
  employee1 match {
    case Employee(firstName, _, _) => println(s"First name is $firstName")
    // First name is john
  }

  // wrong order!
  employee1 match {
    case Employee(lastName, firstName, ssn) => println(s"First name is $firstName, last name is $lastName and ssn is $ssn")
    // First name is wick, last name is john and ssn is 111–11–1111
  }

  final case class WideCaseClass(name: String, s1: String, s2: String, s3: String,
                                 r1: String, r2: String, r3: String, t1: String, t2: String, t3: String)

  val wideClass: WideCaseClass = WideCaseClass("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
  wideClass match
    case WideCaseClass(_, _, _, _, _, r1, _, _, _, _) => println(r1)

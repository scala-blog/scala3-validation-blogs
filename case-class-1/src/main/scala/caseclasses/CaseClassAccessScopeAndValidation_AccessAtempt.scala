package caseclasses

import caseclasses.CaseClassAccessScopeAndValidation.EmployeePrivate

object CaseClassAccessScopeAndValidation_AccessAtempt extends App :
  // The following will create a compile error
  // val employee = EmployeePrivate("John", "Wick", "111-11-1111")
  // method apply cannot be accessed as a member of CaseClassAccessScopeAndValidation.EmployeePrivate.
  // type from module class CaseClassAccessScopeAndValidation_AccessAtempt$.

  val employee2 = EmployeePrivate.fromAllFields("John", "Wick", "111-11-1111")

package caseclasses

object WideCaseClass {
  case class Employee(firstName: String, lastName: String, ssn: String)


  import io.circe.*
  import io.circe.generic.auto.*
  import io.circe.parser.*
  import io.circe.syntax.*

  case class Day(hour1: Int, hour2: Int, hour3: Int, hour4: Int, hour5: Int, hour6: Int,
                 hour7: Int, hour8: Int, hour9: Int, hour10: Int, hour11: Int, hour12: Int,
                 hour13: Int, hour14: Int, hour15: Int, hour16: Int, hour17: Int, hour18: Int,
                 hour19: Int, hour20: Int, hour21: Int, hour22: Int, hour23: Int, hour24: Int)

  val day = Day(20, 30, 40, 50, 60, 70,
    120, 130, 140, 150, 160, 170,
    220, 230, 240, 250, 260, 270,
    320, 330, 340, 350, 360, 370)
  val json = day.asJson


}

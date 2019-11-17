package options

final case class Options(
    scalaTSI: Boolean,
    upickle: Boolean,
    playJson: Boolean,
    optionalFieldAsOptions: Boolean,
    defaultsForTypes: Map[String, String] = Map(
      "String" -> "",
      "Int" -> "0",
      "Double" -> "0.0"
    )
)

object Options {

  def default: Options = Options(
    scalaTSI = true,
    upickle = true,
    playJson = true,
    optionalFieldAsOptions = true
  )

}

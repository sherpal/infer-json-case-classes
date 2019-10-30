package options

import impl.Tree.Leaf

final case class Options(
    scalaTSI: Boolean,
    upickle: Boolean,
    playJson: Boolean,
    optionalFieldAsOptions: Boolean,
    defaultsForTypes: Map[Leaf, String] = Map()
)

object Options {

  def default: Options = Options(
    scalaTSI = true,
    upickle = true,
    playJson = true,
    optionalFieldAsOptions = true
  )

}

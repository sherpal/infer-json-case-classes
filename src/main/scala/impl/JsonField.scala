package impl

import options.Options

final case class JsonField(name: String, subTree: Tree, isList: Boolean, isOptional: Boolean) {
  def typeName(options: Options): String = {
    def encloseList(n: String) = if (isList) s"List[$n]" else n
    def encloseOption(n: String) =
      (isOptional, options.optionalFieldAsOptions, isList) match {
        case (false, _, _) => n
        case (true, true, _) => s"Option[$n]"
        case (true, false, true) => s"$n = Nil"
        case (true, false, false) => s"$n = ${options.defaultsForTypes.getOrElse(n, "???")}"
      }
    encloseOption(encloseList(subTree.name))
  }

  def isLeaf: Boolean = subTree.isLeaf
}

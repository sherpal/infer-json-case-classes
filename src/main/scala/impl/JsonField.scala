package impl

import options.Options

final case class JsonField(name: String, subTree: Tree, isList: Boolean, isOptional: Boolean) {
  def typeName(options: Options): String = {
    def encloseList(n: String) = if (isList) s"List[$n]" else n
    def encloseOption(n: String) =
      if (options.optionalFieldAsOptions && isOptional) s"Option[$n]"
      else if (isOptional && isList) s"$n = Nil"
      else if (isOptional) s"$n = ${options.defaultsForTypes.getOrElse(n, "???")}"
      else n
    encloseOption(encloseList(subTree.name))
  }

  def isLeaf: Boolean = subTree.isLeaf
}

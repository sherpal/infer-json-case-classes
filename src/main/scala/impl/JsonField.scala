package impl

final case class JsonField(name: String, subTree: Tree, isList: Boolean, isOptional: Boolean) {
  def typeName(optionalAsOption: Boolean): String = {
    def encloseList(n: String) = if (isList) s"List[$n]" else n
    def encloseOption(n: String) = if (optionalAsOption && isOptional) s"Option[$n]" else n
    encloseOption(encloseList(subTree.name))
  }

  def isLeaf: Boolean = subTree.isLeaf
}

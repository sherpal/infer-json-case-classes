package impl

import ujson.Value

sealed trait Tree {

  final def isLeaf: Boolean = this.isInstanceOf[Tree.Leaf]

  val name: String

  def isList: Boolean

  final def typeName: String = if (isList) s"List[$name]" else name

  val children: Map[Tree.FieldName, Tree]

  final def display(packagePath: String): String =
    s"""
       |package $packagePath
       |
       |import nl.codestar.scalatsi.{DefaultTSTypes, TSIType, TSType}
       |import play.api.libs.json.{Json, Reads, Writes}
       |
       |final case class $name(
       |${children.toList.map { case (fieldName, tpe) => "\t" + fieldName + ": " + tpe.typeName } .mkString(",\n")}
       |)
       |
       |object $name extends DefaultTSTypes {
       |  import upickle.default.{macroRW, ReadWriter}
       |  implicit val readWriter: ReadWriter[$name] = macroRW
       |
       |  implicit final val reader: Reads[$name] = Json.reads[$name]
       |  implicit final val writer: Writes[$name] = Json.writes[$name]
       |  implicit final val serializer: TSIType[$name] = TSType.fromCaseClass[$name]
       |}
       |""".stripMargin

  final def allDisplays(packagePath: String): List[String] =
    display(packagePath) +: children.values.filterNot(_.isLeaf).flatMap(_.allDisplays(packagePath)).toList

  final def namedAllDisplays(packagePath: String): List[(String, String)] =
    (name -> display(packagePath)) +: children.values.filterNot(_.isLeaf).flatMap(_.namedAllDisplays(packagePath)).toList

}


object Tree {

  type FieldName = String

  trait Leaf extends Tree {
    final val children: Map[FieldName, Tree] = Map()
  }

  case object StringLeaf extends Leaf {
    val name: String = "String"
    def isList: Boolean = false
  }

  case object IntLeaf extends Leaf {
    val name: String = "Int"
    def isList: Boolean = false
  }

  case object DoubleLeaf extends Leaf {
    val name: String = "Double"
    def isList: Boolean = false
  }

  case object BooleanLeaf extends Leaf {
    val name: String = "Boolean"
    def isList: Boolean = false
  }

  case object EmptyJson extends Leaf {
    val name: String = "Empty"
    def isList: Boolean = false
  }

  case object NullLeaf extends Leaf {
    val name: String = "String" // assume it's null
    def isList: Boolean = false
  }

  final class ListLeaf(leaf: Leaf) extends Leaf {
    def isList: Boolean = true
    val name: String = leaf.name
  }

  final class Node(val name: String, val children: Map[FieldName, Tree], val isList: Boolean) extends Tree

  def apply(name: String, children: Map[FieldName, Tree], isList: Boolean): Tree = new Node(name, children, isList)

  def parseValue(name: String, value: Value.Value): Tree = {

    def maybeInt: Option[IntLeaf.type] = value.numOpt.filter(x => x.toInt.toDouble == x).map(_ => IntLeaf)
    def maybeDouble: Option[DoubleLeaf.type] = value.numOpt.map(_ => DoubleLeaf)
    def maybeString: Option[StringLeaf.type] = value.strOpt.map(_ => StringLeaf)
    def maybeBoolean: Option[BooleanLeaf.type] = value.boolOpt.map(_ => BooleanLeaf)
    def maybeNull: Option[NullLeaf.type] = Some(NullLeaf).filter(_ => value.isNull)
    def maybeObj: Option[Map[String, Value.Value]] = value.objOpt.map(_.toMap)
    def maybeList: Option[List[Value.Value]] = value.arrOpt.map(_.toList)

    LazyList(maybeString, maybeBoolean, maybeInt, maybeDouble, maybeList, maybeObj, maybeNull)
      .find(_.isDefined).flatten match {
      case Some(leaf: Leaf) =>
        leaf

      case Some(subValue: Map[String @unchecked, Value.Value @unchecked]) =>
        val children = subValue.toList
          .map { case (key, v) => (key, (key.capitalize, v)) }
          .toMap
          .view.mapValues((parseValue _).tupled)
          .toMap

        Tree(name, children, isList = false)

      case Some(ls: List[Value.Value @unchecked]) =>
        ls match {
          case head :: _ =>
            val treeInList = parseValue(name, head)
            Tree(name, treeInList.children, isList = true)

          case Nil => // if list is empty, assume list of strings
            new ListLeaf(StringLeaf)
        }

      case _ =>
        throw new Exception(s"Could not parse this value: $value")
    }

  }


}

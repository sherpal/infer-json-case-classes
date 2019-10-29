package impl

import ujson.Value

sealed trait Tree {

  final def isLeaf: Boolean = this.isInstanceOf[Tree.Leaf]

  val name: String

  val children: Map[Tree.FieldName, JsonField]

  final def display(packagePath: String): String =
    s"""
       |package $packagePath
       |
       |import nl.codestar.scalatsi.{DefaultTSTypes, TSIType, TSType}
       |import play.api.libs.json.{Json, Reads, Writes}
       |
       |final case class $name(
       |${children.toList
         .map { case (fieldName, field) => "\t" + fieldName + ": " + field.typeName(true) }
         .mkString(",\n")}
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
    display(packagePath) +: children.values.filterNot(_.isLeaf).flatMap(_.subTree.allDisplays(packagePath)).toList

  final def namedAllDisplays(packagePath: String): List[(String, String)] =
    (name -> display(packagePath)) +: children.values
      .filterNot(_.isLeaf)
      .flatMap(_.subTree.namedAllDisplays(packagePath))
      .toList

}

object Tree {

  type FieldName = String

  trait Leaf extends Tree {
    final val children: Map[FieldName, JsonField] = Map()
  }

  case object StringLeaf extends Leaf {
    val name: String = "String"
  }

  case object IntLeaf extends Leaf {
    val name: String = "Int"
  }

  case object DoubleLeaf extends Leaf {
    val name: String = "Double"
  }

  case object BooleanLeaf extends Leaf {
    val name: String = "Boolean"
  }

  case object EmptyJson extends Leaf {
    val name: String = "Empty"
  }

  case object NullLeaf extends Leaf {
    val name: String = "String" // assume it's String
  }

  final class Node(val name: String, val children: Map[FieldName, JsonField]) extends Tree

  def apply(name: String, children: Map[FieldName, JsonField]): Tree = new Node(name, children)

  def parseValue(name: String, value: Value.Value): JsonField = {

    def maybeInt: Option[IntLeaf.type] = value.numOpt.filter(x => x.toInt.toDouble == x).map(_ => IntLeaf)
    def maybeDouble: Option[DoubleLeaf.type] = value.numOpt.map(_ => DoubleLeaf)
    def maybeString: Option[StringLeaf.type] = value.strOpt.map(_ => StringLeaf)
    def maybeBoolean: Option[BooleanLeaf.type] = value.boolOpt.map(_ => BooleanLeaf)
    def maybeNull: Option[NullLeaf.type] = Some(NullLeaf).filter(_ => value.isNull)
    def maybeObj: Option[Map[String, Value.Value]] = value.objOpt.map(_.toMap)
    def maybeList: Option[List[Value.Value]] = value.arrOpt.map(_.toList)

    LazyList(maybeString, maybeBoolean, maybeInt, maybeDouble, maybeList, maybeObj, maybeNull)
      .find(_.isDefined)
      .flatten match {
      case Some(leaf: Leaf) =>
        JsonField(name, leaf, isList = false, isOptional = false)

      case Some(subValue: Map[String @unchecked, Value.Value @unchecked]) =>
        val children = subValue.toList.map { case (key, v) => (key, parseValue(key.capitalize, v)) }.toMap

        JsonField(name, Tree(name, children), isList = false, isOptional = false)

      case Some(ls: List[Value.Value @unchecked]) =>
        ls match {
          case head :: _ =>
            val treeInList = parseValue(name, head)
            treeInList.copy(isList = true)

          case Nil => // if list is empty, assume list of strings
            JsonField(name, StringLeaf, isList = true, isOptional = false)
        }

      case _ =>
        throw new Exception(s"Could not parse this value: $value")
    }

  }

  def collapseTrees(trees: List[Tree]): Option[Tree] = trees match {
    case Nil               => None
    case (head: Leaf) :: _ => Some(head)
    case list =>
      val numTrees = list.length
      val newChildren = list
        .flatMap(_.children.toList)
        .groupBy(_._1)
        .toList
        .map {
          case (fieldName, children) =>
            val jsonFields = children.map(_._2)
            (
              fieldName,
              collapseTrees(jsonFields.map(_.subTree)).get,
              jsonFields.head.isList,
              jsonFields.exists(_.isOptional) || children.length < numTrees
            )
        }
        .map((JsonField.apply _).tupled)
        .map { case jf @ JsonField(name, _, _, _) => name -> jf }
        .toMap

      Some(
        Tree(list.head.name, newChildren)
      )

  }

}

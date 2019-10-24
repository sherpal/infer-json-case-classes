package impl

import ujson.Value

import scala.util.{Success, Try}

sealed trait Tree {

  final def isLeaf: Boolean = this.isInstanceOf[Tree.Leaf]

  val name: String

  def isList: Boolean

  def typeName: String = if (isList) s"List[$name]" else name

  val children: Map[Tree.FieldName, Tree]

  def display(packagePath: String): String =
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

  def allDisplays(packagePath: String): List[String] =
    display(packagePath) +: children.values.filterNot(_.isLeaf).flatMap(_.allDisplays(packagePath)).toList

  def namedAllDisplays(packagePath: String): List[(String, String)] =
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

  case object EmptyJson extends Leaf {
    val name: String = "Empty"
    def isList: Boolean = false
  }

  final class ListLeaf(leaf: Leaf) extends Leaf {
    def isList: Boolean = true
    val name: String = leaf.name
  }

  final class Node(val name: String, val children: Map[FieldName, Tree], val isList: Boolean) extends Tree

  def apply(name: String, children: Map[FieldName, Tree], isList: Boolean): Tree = new Node(name, children, isList)


  def parseValue(name: String, value: Value.Value): Tree = {

    def tryObj: Try[Map[String, Value.Value]] = Try(value.obj.toMap)
    def tryInt: Try[Int] = Try(value.num).filter(x => x.toInt.toDouble == x).map(_.toInt)
    def tryDouble: Try[Double] = Try(value.num)
    def tryList: Try[List[Value.Value]] = Try(value.arr.toList)
    def tryString: Try[String] = Try(value.str)

    LazyList(tryString, tryInt, tryDouble, tryList, tryObj)
      .find(_.isSuccess) match {
      case Some(Success(_: String)) =>
        StringLeaf

      case Some(Success(_: Int)) =>
        IntLeaf

      case Some(Success(_: Double)) =>
        DoubleLeaf

      case Some(Success(subValue: Map[String @unchecked, Value.Value @unchecked])) =>
        val children = subValue.toList
          .map { case (key, v) => (key, (key.capitalize, v)) }
          .toMap
          .view.mapValues((parseValue _).tupled)
          .toMap

        Tree(name, children, isList = false)

      case Some(Success(ls: List[Value.Value @unchecked])) =>
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

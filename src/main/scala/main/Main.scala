package main

import impl.Tree
import options.Options

object Main {

  def main(args: Array[String]): Unit = {

//    val str =
//      """{"coord":{"lon":4.35,"lat":50.85},"weather":[{"id":802,"main":"Clouds","description":"scattered clouds","icon":"03d"}],"base":"stations","main":{"temp":289.88,"pressure":1011,"humidity":77,"temp_min":288.71,"temp_max":291.48},"visibility":10000,"wind":{"speed":2.1,"deg":90},"clouds":{"all":33},"dt":1571844327,"sys":{"type":1,"id":1227,"country":"BE","sunrise":1571811487,"sunset":1571848532},"timezone":7200,"id":2800866,"name":"Brussels","cod":200}"""
//
//    val packagePath = "models.openweather"
//    val jsonName = "CurrentWeatherData"
//
//    val json = ujson.read(str)
//
//    println(ujson.write(json, indent = 2))
//
//    val tree = Tree.parseValue(jsonName, json)
//    os.list(os.pwd / "output").foreach(os.remove)
//    val namedDisplays = tree.subTree.namedAllDisplays(packagePath).toMap
//
//    namedDisplays.toList.foreach { case (name, display) =>
//      val fileName = s"$name.scala"
//        os.write(os.pwd / "output" / fileName, display)
//    }

    val str1 =
      """
        |{
        | "hello": "coucou",
        | "truc": 3
        |}
        |""".stripMargin
    val str2 =
      """
        |{
        | "hello": "cluche",
        | "other": ["un", "deux"]
        |}
        |""".stripMargin

    val json1 = ujson.read(str1)
    val json2 = ujson.read(str2)

    val field1 = Tree.parseValue("foo", json1)
    val field2 = Tree.parseValue("foo", json2)

    println(
      Tree
        .collapseTrees(List(field1.subTree, field2.subTree))
        .get
        .display("bar", Options.default.copy(scalaTSI = false, optionalFieldAsOptions = true))
    )

  }

}

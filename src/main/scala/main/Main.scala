package main

import impl.Tree

object Main {


  def main(args: Array[String]): Unit = {

    val str =
      """{"coord":{"lon":4.35,"lat":50.85},"weather":[{"id":802,"main":"Clouds","description":"scattered clouds","icon":"03d"}],"base":"stations","main":{"temp":289.88,"pressure":1011,"humidity":77,"temp_min":288.71,"temp_max":291.48},"visibility":10000,"wind":{"speed":2.1,"deg":90},"clouds":{"all":33},"dt":1571844327,"sys":{"type":1,"id":1227,"country":"BE","sunrise":1571811487,"sunset":1571848532},"timezone":7200,"id":2800866,"name":"Brussels","cod":200}"""

    val packagePath = "models.openweather"
    val jsonName = "Open"

    val json = ujson.read(str)

    println(ujson.write(json, indent = 2))

    val tree = Tree.parseValue(jsonName, json)
    os.list(os.pwd / "output").foreach(os.remove)
    val namedDisplays = tree.namedAllDisplays(packagePath).toMap

    namedDisplays.toList.foreach { case (name, display) =>
      val fileName = s"$name.scala"
        os.write(os.pwd / "output" / fileName, display)
    }

  }

}

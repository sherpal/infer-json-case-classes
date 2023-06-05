# Infer JSON case classes

The goal of this project is, given the json representation of an object, automatically create all the case classes that will represent that object.

This is supposed to be useful when writing case classes representations of external APIs.

The script generates all the files needed for the case classes, "inside" the given package. It bundles
- `ReadWriter` for [upickle](http://www.lihaoyi.com/upickle/)
- `Reads` and `Writes` for [play json](https://github.com/playframework/play-json).
- [scala-tsi](https://github.com/code-star/scala-tsi) needed config

## Example

The following Json comes from a API call to [open weather api](https://openweathermap.org/):
```
{
  "coord": {
    "lon": 4.35,
    "lat": 50.85
  },
  "weather": [
    {
      "id": 802,
      "main": "Clouds",
      "description": "scattered clouds",
      "icon": "03d"
    }
  ],
  "base": "stations",
  "main": {
    "temp": 289.88,
    "pressure": 1011,
    "humidity": 77,
    "temp_min": 288.71,
    "temp_max": 291.48
  },
  "visibility": 10000,
  "wind": {
    "speed": 2.1,
    "deg": 90
  },
  "clouds": {
    "all": 33
  },
  "dt": 1571844327,
  "sys": {
    "type": 1,
    "id": 1227,
    "country": "BE",
    "sunrise": 1571811487,
    "sunset": 1571848532
  },
  "timezone": 7200,
  "id": 2800866,
  "name": "Brussels",
  "cod": 200
}
```

When running the script on it (with package name `models.openweather`), you'll get in the `output` directory, the following files:
- `Clouds.scala`
- `Coord.scala`
- `Main.scala`
- `Open.scala` (this is the name we gave to the whole Json)
- `Sys.scala`
- `Weather.scala`
- `Wind.scala`

For example, the content of `Open.scala` will be
```
package models.openweather

import nl.codestar.scalatsi.{DefaultTSTypes, TSIType, TSType}
import play.api.libs.json.{Json, Reads, Writes}

final case class Open(
	name: String,
	dt: Int,
	timezone: Int,
	id: Int,
	weather: List[Weather],
	visibility: Int,
	cod: Int,
	main: Main,
	sys: Sys,
	wind: Wind,
	base: String,
	coord: Coord,
	clouds: Clouds
)

object Open extends DefaultTSTypes {
  import upickle.default.{macroRW, ReadWriter}
  implicit val readWriter: ReadWriter[Open] = macroRW

  implicit final val reader: Reads[Open] = Json.reads[Open]
  implicit final val writer: Writes[Open] = Json.writes[Open]
  implicit final val serializer: TSIType[Open] = TSType.fromCaseClass[Open]
}
```

## Upcoming

The following things should also be implemented:
- whole battery of tests
- `@JSExport` for use with [Scala.js](https://www.scala-js.org/)

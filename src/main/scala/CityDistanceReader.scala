import scala.io.Source

object CityDistanceReader {
    def getMap(path: String): Map[(City, City), Distance] = {
        val lines = Source.fromFile(path).getLines().toList

        val cities = lines.head.trim.split("\\s+").toList

        lines.tail.flatMap { line =>
            val values = line.trim.split("\\s+").toList
            val city1 = values.head
            val distances = values.tail.map(_.toInt)

            cities.zip(distances).map { case (city2, distance) =>
                (city1, city2) -> distance
            }
        }.toMap
    }
    def getCities(path: String): List[City] = {
        val lines = Source.fromFile(path).getLines().toList
        lines.head.trim.split("\\s+").toList

    }
}

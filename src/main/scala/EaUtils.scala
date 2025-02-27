import scala.util.Random

object EaUtils {
    def randomSolution(cities: List[City]): Solution = Random.shuffle(cities)

    def fitnessFun(map: Map[(City, City), Distance])(
        solution: Solution
    ): Score =
        val roundTrip = (solution :+ solution.head)
        roundTrip
            .sliding(2)
            .map {
                case List(cur, next) =>
                    map.getOrElse((cur, next), (Int.MaxValue))
                case _ => Int.MaxValue
            }
            .sum

    def mutate(mutProb: Double)(s: Solution): Solution = {
        s.map { gene =>
            {
                if Random.between(0.0, 1.0) > mutProb then gene
                else s(Random.between(0, s.length))
            }
        } 

    }

}

import scala.util.Random

object Utils {
    def randomSolution(cities: List[City]): Solution = Random.shuffle(cities)
    def initPop(cities: List[City])(n: Int): Population =
        (0 until 100).toList.map(_ => randomSolution(cities))
    def isValid(solution: Solution): Boolean =
        solution.distinct.length == solution.length

    def mapFit(f: Solution => Score)(pop: Population): MappedPopulation =
        pop.map(s => (s, f(s)))

    def populationAvgFitness(mpop: MappedPopulation): Score =
        mpop.map(x => x._2).reduce((a, b) => a + b) / mpop.length
    def populationEliteFitness(mpop: MappedPopulation): Score =
        mpop.maxBy(_._2)._2

}

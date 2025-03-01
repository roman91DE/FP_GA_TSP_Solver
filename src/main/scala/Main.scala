val conf = Configurations(
    mutProb = .05,
    coProb = .9,
    popSize = 2000,
    tournSize = 10,
    generations = 100
)

object Main {
    def main(args: Array[String]): Unit = {
        val dataPath = "./src/main/resources/citiesAndDistances.txt"
        val map: Map[(City, City), Distance] =
            CityDistanceReader.getMap(dataPath)
        val cities: List[City] = CityDistanceReader.getCities(dataPath)

        implicit val fitnessFunction: List[String] => Int =
            DistanceFitnessOperator(map).fitness
        implicit val mutationFunction: List[String] => List[String] =
            SwapMutationOperator(conf.mutProb).mutate
        implicit val crossoverFunction
            : (List[String], List[String]) => List[String] =
            OrderedCrossoverOperator(conf.coProb).crossover
        implicit val selectionFunction: List[List[String]] => List[String] =
            TournamentSelectionOperator(conf.tournSize, fitnessFunction).select

        val pop = Utils.initPop(cities)(conf.popSize)

        val solution = evolve(pop, 0)
        println(s"Best Solution: ${solution}")
        println(s"Fitness: ${fitnessFunction(solution)}")
    }

    def evolve(pop: Population, currentGen: Int)(implicit
        fitnessFunction: List[String] => Int,
        mutationFunction: List[String] => List[String],
        crossoverFunction: (List[String], List[String]) => List[String],
        selectionFunction: List[List[String]] => List[String]
    ): Solution = {
        if (currentGen == conf.generations) pop.maxBy(fitnessFunction)
        else {
            val newPop = (0 until conf.popSize).map { _ =>
                val parent1 = selectionFunction(pop)
                val parent2 = selectionFunction(pop)
                val child = crossoverFunction(parent1, parent2)
                mutationFunction(child)
            }.toList

            val mpop = Utils.mapFit(fitnessFunction)(pop)
            println(s"Generation: $currentGen")
            println(s"Population Avg. Fitness: ${Utils.populationAvgFitness(mpop)}")
            println(s"Elite Fitness: ${Utils.populationEliteFitness(mpop)}")

            evolve(newPop, currentGen + 1)
        }
    }
}

object Main {
    def main(args: Array[String]): Unit = {
        val dataPath = "./src/main/resources/citiesAndDistances.txt"
        val map: Map[(City, City), Distance] =
            CityDistanceReader.getMap(dataPath)
        val cites: List[City] = CityDistanceReader.getCities(dataPath)
        val fitness = EaUtils.fitnessFun(map)

        var s = EaUtils.randomSolution(cites)
        val mutProb = .1
        val appMutation = EaUtils.mutate(mutProb)
        for (i <- 1 to 1000) {
            
            val mutated = appMutation(s)
            var f = fitness(mutated)

            println(s"Solution: $mutated")
            println(s"Fitness: $f")
        }

    }
}

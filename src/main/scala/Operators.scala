import scala.util.Random
import Operators.FitnessOperator

object Operators {

    trait FitnessOperator {
        def fitness(solution: Solution): Score
    }

    trait MutationOperator {
        def mutate(solution: Solution): Solution
    }

    trait CrossoverOperator {
        def crossover(parent1: Solution, parent2: Solution): Solution
    }

    trait SelectionOperator {
      def select(pop: Population): Solution
    }


}

class DistanceFitnessOperator(map: Map[(City, City), Distance])
    extends Operators.FitnessOperator {
    override def fitness(solution: Solution): Score = {
        val roundTrip = solution :+ solution.head
        -1 * roundTrip
            .sliding(2)
            .map {
                case List(cur, next) => map.getOrElse((cur, next), Int.MaxValue)
                case _               => Int.MaxValue
            }
            .sum
    }
}

class SwapMutationOperator(mutProb: Double) extends Operators.MutationOperator {
    override def mutate(solution: Solution): Solution = {
      if Random().between(.0, 1.0) > solution.length * mutProb then solution else
        val i = randomIndex(solution)
        val j = randomIndex(solution)
        swap(solution, i ,j)
    }

    private def randomIndex(sol: Solution): Int = Random.between(0, sol.length)
    private def swap(sol: Solution, i: Int, j: Int): Solution = {
        if i == j then sol else sol.updated(i, sol(j)).updated(j, sol(i))
    }

}

class OrderedCrossoverOperator(coProb: Double)
    extends Operators.CrossoverOperator {
    override def crossover(a: Solution, b: Solution): Solution = {
        if (Random.nextDouble() > coProb) a
        else {
            val pivot = Random.between(1, a.length)
            val left = a.take(pivot).toSet
            val right = b.filterNot(left)
            a.take(pivot) ::: right
        }
    }
}


class TournamentSelectionOperator(tournSize: Int, fitnessFunction: (Solution => Score))
    extends Operators.SelectionOperator {
    private def getTournPool(pop: Population): Population = Random.shuffle(pop).take(tournSize)
    override def select(pop: Population): Solution = {
      getTournPool(pop).sortBy(fitnessFunction(_)).reverse.head

    }
}

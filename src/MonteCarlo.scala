import scala.util.{Random}
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._

object StateGraph {
  val initialState = "1111"
  val finalState = "0000"

  def generateValidStates( source: String ) : List[String] = {
    //Change state given list of objects
    def  moveObjects(value: String, positions: List[Int]) : String = {
      //Change state
      def move( item: Char ) : Char = if(item == '1')  return '0' else return '1'
      return positions.foldLeft(value)((s, i) => s.updated(i, move(value.charAt(i))))
    }

    //Mapping across possible objects: cabbage, goat or wolf
    val possibleStates = List(1,2,3).map(item => {
      //Farmer can only move objects which are on his side
      if(source.charAt(0) == source.charAt(item)) moveObjects(source, List(0, item))
      else ""
    })
    //Add the state in which the farmer crosses the river alone
    return possibleStates.filter(x => x != "") ::: List(moveObjects(source, List(0)))
  }

  def checkFinalState(state: String) : Boolean = {
    return state.equals(finalState)
  }

  def checkBadState(state: String) : Boolean = {
    if((state.charAt(0) != state.charAt(1)) && (state.charAt(1) == state.charAt(2))) return true
    else if((state.charAt(0) != state.charAt(2)) && (state.charAt(2) == state.charAt(3))) return true
    else return false
  }

  //FIX: check if final state is before (paths with less then n+2)
  def checkIfTraceValid(trace: List[String]) : Boolean = {
    return trace.filter(x => !checkBadState(x)).length == trace.length && checkFinalState(trace.last)
  }

}

class MonteCarlo(nIter: Int, nCores: Int, pathSizeLimit: Int) {
  val graph = StateGraph

  def run(): IndexedSeq[Boolean] = {
    val futures = for {
      c <- 0 until nCores
    } yield {
      Future {
        for {c <- 0 until nIter} yield {
          graph.checkIfTraceValid(randomWalker()) }
      }
    }

    val results = Future.sequence(futures)
    return Await.result(results, Duration.Inf).flatten
  }

  def randomWalker() : List[String]  = {
    def getNextNeighbor(source: String) : String = {
      val neighbors = graph.generateValidStates(source)
      //Randomly choose next state with uniform probability
      val r = Random.nextInt(neighbors.size)
      val nextState = neighbors(r)
      return nextState
    }
    Stream.iterate(graph.initialState)(getNextNeighbor)
      .take(pathSizeLimit).toList
  }
}


object Main extends App {
  val nIter = 25000
  val nCores = 4
  val pathSizeLimit = 10

  val simulator = new MonteCarlo(nIter, nCores, pathSizeLimit)
  val results = simulator.run()

  println("Valid Trace Probability",results.filter(x => x == true).length/ results.length)

  //List("0111", "0110", "1101", "1001", "0001").foreach(x => println(generateValidStates(x)))
}

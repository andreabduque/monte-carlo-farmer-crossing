import scala.util.{Failure, Random, Success}
import Stream._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._

class MonteCarlo {

}

//mutable -> list of edges
//methods
//generate new state (with uniform probability of choosing between all possible combinations)
//check if valid state
//returns trace
class Trace {

}


//function that generate valid traces given N -> acho que nao precisa

//function that checks if a trace is valid
//function that generates random traces of maximum size N+2 (stops when something is eaten or final state)

object Hello extends App {

  //Change state given list of objects
  def  moveObjects(value: String, positions: List[Int]) : String = {
    //Change state
    def move( item: Char ) : Char = {
      if(item == '1')  return '0' else return '1'
    }
    return positions.foldLeft(value)((s, i) => s.updated(i, move(value.charAt(i))))
  }

  def generateValidStates( source: String ) : List[String] = {
    //Mapping across possible objects: cabbage, goat or wolf
    val possibleStates = List(1,2,3).map(item => {
      //Farmer can only move objects which are on his side
      if(source.charAt(0) == source.charAt(item)) moveObjects(source, List(0, item))
      else ""
    })

    //Add the state in which the farmer crosses the river alone
    return possibleStates.filter(x => x != "") ::: List(moveObjects(source, List(0)))
  }

  def checkEndTrace(state: String) : Boolean = {
      if (state.equals("0000")) return true
      else if((state.charAt(0) != state.charAt(1)) && (state.charAt(1) == state.charAt(2))) return true
      else if((state.charAt(0) != state.charAt(2)) && (state.charAt(2) == state.charAt(3))) return true
      else return false
  }

  def checkFinalState(state: String) : Boolean = {
    return state.equals("0000")
  }

  def checkBadState(state: String) : Boolean = {
    if((state.charAt(0) != state.charAt(1)) && (state.charAt(1) == state.charAt(2))) return true
    else if((state.charAt(0) != state.charAt(2)) && (state.charAt(2) == state.charAt(3))) return true
    else return false
  }

  def checkIfTraceValid(trace: List[String]) : Boolean = {
    return trace.filter(x => !checkBadState(x)).length == trace.length && checkFinalState(trace.last)
  }

  def getNextNeighbor(source: String) : String = {
    val neighbors = generateValidStates(source)
    //Randomly choose next state
    val r = Random.nextInt(neighbors.size)
    val nextState = neighbors(r)
    return nextState
  }

  def randomWalker(pathSizeLimit: Int) : List[String]  = {
    var initialState = "1111"
    val walker = Stream.iterate(initialState)(getNextNeighbor).take(pathSizeLimit)
    return walker.toList
  }

  //println(randomWalker(10))




  val nIter = 100000
  val pathSizeLimit = 10
//
//  val fu = Future {
//    //for {c <- 0 until nIter} yield {
//      checkIfTraceValid(randomWalker(pathSizeLimit))
//  }
//    fu.onComplete(r => println(r))
//  fu.foreach(r => print())

//
//  for {c <- 0 until nIter} yield {
//    println("loop working")
//    checkIfTraceValid(randomWalker(pathSizeLimit))}
//}
//
//  //for with 4 threads -> each future returns a valid/not valid array. join them
  val nCores = 4
//
  val futures = for {
    c <- 0 until nCores
  } yield {
    Future {
      for {c <- 0 until nIter} yield {
        checkIfTraceValid(randomWalker(pathSizeLimit)) }
    }
}

  val results = Future.sequence(futures)

  val all = Await.result(results, scala.concurrent.duration.Duration.Inf).flatten

  //all.foreach(x => println(x))
  println(all.length)
  println("valid traces")
  println(all.filter(x => x == true).length)

//  println("testeee")
//
//  //1 if the farmer or object is on the initial side, 0 if on the other side of the river
//  var initialState = "1111"
//  var finalState = "0000"

  //List("0111", "0110", "1101", "1001", "0001").foreach(x => println(generateValidStates(x)))
}

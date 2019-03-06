class MonteCarlo {

}

//mutable -> list of edges
//methods
//generate new state (with uniform probability of choosing between all possible combinations)
//check if valid state
//returns trace
class Trace {

}

//true if it is on the initial side of the river, false if it is on the other side
//case class State(farmer: Boolean, wolf: Boolean, goat: Boolean, cabbage: Boolean)

//Represents one path between two states in the state graph
//case class Edge(source: State, target: State)

//function that generates a state given one state
//(function that generates all possible combinations given a state)

//receives state returns list with all combinations


//variable that keeps all the states
//function that checks if that state is valid
//function that generates paths

//case class State(value: String, isValid: Boolean)

//object Item extends Enumeration {
//  type Item = Value
//  val FARMER, WOLF, GOAT, CABBAGE = Value
//}



//replace to change state
//loop over with zip
//loop over all possible combinations


//
////check if state is valid
//def checkState()

object Hello extends App {

  //Change object position
  def move( item: Char ) : Char = {
    if(item == '1')  return '0' else return '1'
  }

  //Move more than one object
  def  moveObjects(value: String, positions: List[Int]) : String = {
    return positions.foldLeft(value)((s, i) => s.updated(i, move(value.charAt(i))))
  }


  def generateValidStates( source: String ) : List[String] = {

    //Mapping across possible objects: wolf, goat or cabbage
    val possibleStates = List(1,2,3).map(item => {
      //Farmer can only move objects which are on his side
      if(source.charAt(0) == source.charAt(item)) moveObjects(source, List(0,item))
      else ""
    })

    //The farmer can always cross the river alone
    return possibleStates.filter(x => x != "") ::: List(source.updated(0, move(source.charAt(0))))
  }

  var initialState = "1111"
  var finalState = "0000"

  List("0111", "0110", "1101", "1001", "0001").foreach(x => println(generateValidStates(x)))
}

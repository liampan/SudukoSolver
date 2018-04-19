package suduko

object SudukoSolver {

  case class Number(num: Option[Int])

  case class Square(numbers : List[Number])

  case class Grid(squares : List[Square])


  def findEmptyCell(square: Square):List[Int] ={
    square
      .numbers
      .zipWithIndex
      .filter(x => x._1 ==Number(None))
      .map(_._2)
  }

  def missingNumberFromSquareFinder(square: Square):List[Int] ={
    val foundNumbers = square.numbers.filter(x => x.num.isDefined)
    val allNumbers = List(1,2,3,4,5,6,7,8,9)
    val differentNumbers = allNumbers.diff(foundNumbers.map(_.num.get))
    differentNumbers
  }

}
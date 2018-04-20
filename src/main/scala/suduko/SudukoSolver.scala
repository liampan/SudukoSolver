package suduko

object SudukoSolver extends App {

  case class Number(num: Option[Int])

  case class Square(numbers : List[Number])

  case class Grid(squares : List[Square])



  def left(list: Square) :List[Number] ={List(list.numbers(0), list.numbers(3), list.numbers(6))}
  def centre(list: Square) :List[Number] ={ List(list.numbers(1),list.numbers(4),list.numbers(7))}
  def right(list: Square) :List[Number] ={ List(list.numbers(2), list.numbers(5),list.numbers(8))}

  def top(list: Square) :List[Number] ={ list.numbers.take(3)}
  def middle(list: Square) :List[Number] ={ list.numbers.slice(3,6)}
  def bottom(list: Square) :List[Number] ={ list.numbers.takeRight(3)}




  def findEmptyCell(square: Square):List[Int] ={

    // do i need this any more?

    square
      .numbers
      .zipWithIndex
      .filter(x => x._1 ==Number(None))
      .map(_._2)
  }

  def missingNumberFromSquareFinder(square: Square):Set[Int] ={
    val foundNumbers = square.numbers.filter(x => x.num.isDefined)
    val allNumbers = List(1,2,3,4,5,6,7,8,9)
    val differentNumbers = allNumbers.diff(foundNumbers.map(_.num.get))
    differentNumbers.toSet
  }

  def missingNumbersFromRow(threeSquares : List[Square], rowPos : Int) :Set[Int]= {
    val allNumbers = List(1,2,3,4,5,6,7,8,9)
    rowPos match {
      case 0 => val out = threeSquares.flatMap(top).filter(_.num.isDefined)
        allNumbers.diff(out.map(_.num.get)).toSet
      case 1 => val out = threeSquares.flatMap(middle).filter(_.num.isDefined)
        allNumbers.diff(out.map(_.num.get)).toSet
      case 2 => val out = threeSquares.flatMap(bottom).filter(_.num.isDefined)
        allNumbers.diff(out.map(_.num.get)).toSet
    }
  }

  def missingNumbersFromColumn(threeSquares : List[Square], columnPos : Int) :Set[Int]= {
    val allNumbers = List(1,2,3,4,5,6,7,8,9)
    columnPos match {
      case 0 => val out = threeSquares.flatMap(left).filter(_.num.isDefined)
        allNumbers.diff(out.map(_.num.get)).toSet
      case 1 =>
        val out = threeSquares.flatMap(centre)
        val outer = out.filter(_.num.isDefined).map(_.num.get)
        allNumbers.diff(outer).toSet
      case 2 => val out = threeSquares.flatMap(right).filter(_.num.isDefined)
        allNumbers.diff(out.map(_.num.get)).toSet
      }
  }

  def vennDiagramForCell(grid: Grid,squarePos : Int, numberPos : Int) : Set[Int]= {
    val columnThreeSquare = squarePos % 3 match {
      case 0 => List(grid.squares(0),grid.squares(3),grid.squares(6))
      case 1 => List(grid.squares(1),grid.squares(4),grid.squares(7))
      case 2 => List(grid.squares(2),grid.squares(5),grid.squares(8))
    }
    val rowThreeSquare : List[Square] = {
      if (squarePos < 3)      List(grid.squares(0),grid.squares(1),grid.squares(2))
      else if (squarePos < 6) List(grid.squares(3),grid.squares(4),grid.squares(5))
      else                     List(grid.squares(6),grid.squares(7),grid.squares(8))
    }
    val rowPos = {
      if (numberPos < 3){0}
      else if (numberPos < 6){1}
      else {2}
    }
    val squareSet = missingNumberFromSquareFinder(grid.squares(squarePos))
    val columnSet = missingNumbersFromColumn(columnThreeSquare,numberPos%3)
    val rowSet = missingNumbersFromRow(rowThreeSquare,rowPos)
    squareSet.intersect(columnSet).intersect(rowSet)
  }

}
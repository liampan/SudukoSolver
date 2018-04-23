package suduko

import suduko._

object SudukoSolver extends App {



  def reset ={
    squarePlacer = 0
    cellPlacer = -1
    parseCounter = 0
  }

  var squarePlacer = 0
  var cellPlacer = -1
  var parseCounter = 0

  def GPS : (Int,Int) ={
    cellPlacer = cellPlacer+1
    if (cellPlacer == 9) {
      cellPlacer = 0
      squarePlacer = squarePlacer+1
    }
    if(squarePlacer == 9) {
      squarePlacer = 0
    }
    (squarePlacer,cellPlacer)
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
      case 0 => val out = threeSquares.flatMap(_.top).filter(_.num.isDefined)
        allNumbers.diff(out.map(_.num.get)).toSet
      case 1 => val out = threeSquares.flatMap(_.middle).filter(_.num.isDefined)
        allNumbers.diff(out.map(_.num.get)).toSet
      case 2 => val out = threeSquares.flatMap(_.bottom).filter(_.num.isDefined)
        allNumbers.diff(out.map(_.num.get)).toSet
    }
  }

  def missingNumbersFromColumn(threeSquares : List[Square], columnPos : Int) :Set[Int]= {
    val allNumbers = List(1,2,3,4,5,6,7,8,9)
    columnPos match {
      case 0 => val out = threeSquares.flatMap(_.left).filter(_.num.isDefined)
        allNumbers.diff(out.map(_.num.get)).toSet
      case 1 =>
        val out = threeSquares.flatMap(_.centre)
        val outer = out.filter(_.num.isDefined).map(_.num.get)
        allNumbers.diff(outer).toSet
      case 2 => val out = threeSquares.flatMap(_.right).filter(_.num.isDefined)
        allNumbers.diff(out.map(_.num.get)).toSet
      }
  }

  def vennDiagramForCell(grid: Grid,squarePos : Int, numberPos : Int) : Number= {
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
    val potential: Set[Int] = squareSet.intersect(columnSet).intersect(rowSet)
    if (potential.size == 1) {Number(Some(potential.head))}
    else Number(None)
  }

  def placer(grid: Grid,squarePos : Int, cellPos : Int, newInt : Number) :Grid = {
    val uuuu = grid.squares
    val box: Square = Square(grid.squares(squarePos).numbers.updated(cellPos, newInt))
    val asdfg: List[Square] = uuuu.updated(squarePos,box)
    grid.copy(squares = asdfg)
  }

  def parser(grid: Grid) : Grid = {
    val gps = GPS
    val Squ = gps._1
    val Cel = gps._2
    if (grid.squares(Squ).numbers(Cel).num.isDefined) {parser(grid)}
    val currentCell = vennDiagramForCell(grid,Squ,Cel)
    if (currentCell.num.isDefined) {
      val newGrid = placer(grid,Squ,Cel,currentCell)
      parser(newGrid)
    }
    else if (parseCounter < 82) {
      parseCounter +=1
      parser(grid)
    }
    else {
      reset
      grid
    }
  }

  val input =
    """
      | . . 4 | 8 . . | . 1 7
      | 6 7 . | 9 . . | . . .
      | 5 . 8 | . 3 . | . . 4
      |-------+-------+-------
      | 3 . . | 7 4 . | 1 . .
      | . 6 9 | . . . | 7 8 .
      | . . 1 | . 6 9 | . . 5
      |-------+-------+-------
      | 1 . . | . 8 . | 3 . 6
      | . . . | . . 6 | . 9 1
      | 2 4 . | . . 1 | 5 . .
    """.stripMargin


  def readyInput(input :String) :Grid = {
    val regex = "[0-9]|[.]".r
    val split = regex.findAllMatchIn(input).toList
      .map(x => if(x.toString==".")Number(None) else Number(Some(x.toString.toInt)))
    val group = split.grouped(3).toList
    val TL = List(group(0),group(3),group(6)).flatten
    val TC = List(group(1),group(4),group(7)).flatten
    val TR = List(group(2),group(5),group(8)).flatten
    val ML = List(group(9),group(12),group(15)).flatten
    val MC = List(group(10),group(13),group(16)).flatten
    val MR = List(group(11),group(14),group(17)).flatten
    val BL = List(group(18),group(21),group(24)).flatten
    val BC = List(group(19),group(22),group(25)).flatten
    val BR = List(group(20),group(23),group(26)).flatten
    val listSquare = List(TL,TC,TR,ML,MC,MR,BL,BC,BR)
    Grid(listSquare.map(Square))
  }

  def printBoard(grid: Grid) ={
    val numSquares = grid.squares.map(x => x.numbers.map(y => if(y.num.isDefined){Console.RED +s"${y.num.get.toString}" + Console.RESET} else "X"))
    val colums = numSquares.transpose.transpose.zipWithIndex
    println(colums.map(x => s"\n ${x._2} =  ${x._1}"))
  }

  def run(input: String) : Unit = {
    val theGrid = readyInput(input)
    printBoard(theGrid)
    val finished = parser(theGrid)
    printBoard(finished)
  }

  run(input)
}
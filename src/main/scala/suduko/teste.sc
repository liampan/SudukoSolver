import suduko.{Grid, Number, Square}

def readyInput(input :String)  = {
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
  Grid(listSquare.map(Square(_)))
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

readyInput(input)


List(1,1,1,2,2,2,3,3,3).sliding(3,3).toList



def squareBuilder(topLeftNum : Int) : Square ={
  val allNumbers : List[Number] = List(1,2,3,4,5,6,7,8,9).map(x => Number(Some(x)))
  val tuple: (List[Number], List[Number]) = allNumbers.splitAt(topLeftNum)
  Square(tuple._2++tuple._1)
}

val complete: Grid = Grid(List(squareBuilder(0), squareBuilder(3), squareBuilder(6),
  squareBuilder(1), squareBuilder(4), squareBuilder(7),
  squareBuilder(2), squareBuilder(5), squareBuilder(8)))


def printBoard(grid: Grid) ={
  val numSquares: List[List[String]] = complete.squares.map(x => x.numbers.map(y => y.num.get.toString))


}

printBoard(complete)
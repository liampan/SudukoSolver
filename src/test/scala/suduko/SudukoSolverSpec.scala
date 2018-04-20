package suduko

import org.scalatest._
import suduko.SudukoSolver._

class SudukoSolverSpec extends WordSpec with MustMatchers{

   def squareBuilder(topLeftNum : Int) : Square ={
     val allNumbers : List[Number] = List(1,2,3,4,5,6,7,8,9).map(x => Number(Some(x)))
     val tuple: (List[Number], List[Number]) = allNumbers.splitAt(topLeftNum)
     Square(tuple._2++tuple._1)
   }

  def squareBuilderNone : Square ={
    val allNumbers : List[Number] = List(0,0,0,0,0,0,0,0,0).map(x => Number(None))
    Square(allNumbers)
  }

  def squareBuilderWithOneMissing(topLeftNum: Int, missingNum : Int) : Square ={
    val allNumbers : List[Number] = List(1,2,3,4,5,6,7,8,9).updated(missingNum-1,0).map(x => if(x ==0){Number(None)} else Number(Some(x)))
    val tuple: (List[Number], List[Number]) = allNumbers.splitAt(topLeftNum)
    Square(tuple._2++tuple._1)
  }

  "EmptyCellFinder" must {

    "return the index of the empty cell in a square with one empty cell" in {
      val test = Square(List(Number(Some(1)), Number(None)))

      SudukoSolver.findEmptyCell(test) mustEqual List(1)
    }

    "return the index of both empty cells in a square with two empty cells" in {

      SudukoSolver.findEmptyCell(Square(List(Number(Some(1)), Number(None), Number(None)))) mustEqual List(1, 2)
    }

    "return the index of all empty cells in a square with some empty cells" in {

      SudukoSolver.findEmptyCell(Square(List(Number(None), Number(None), Number(Some(9))))) mustEqual List(0, 1)
    }
  }


  "MissingNumberFromSquare" must {

    "return an empty list for a square tht is complete" in {


      SudukoSolver.missingNumberFromSquareFinder(squareBuilder(1))  mustEqual Set()
    }

    "return all the numbers for a square that is empty " in {


      SudukoSolver.missingNumberFromSquareFinder(squareBuilderNone)  mustEqual Set(1,2,3,4,5,6,7,8,9)
    }

    "return the number missing from a square that is missing one number " in {


      SudukoSolver.missingNumberFromSquareFinder(squareBuilderWithOneMissing(1,9) )  mustEqual Set(9)
    }

    "return the numbers missing from a square that is missing two numbers" in {


      SudukoSolver.missingNumberFromSquareFinder(
        Square(List(
          Number(Some(1)),
          Number(None),
          Number(Some(3)),
          Number(Some(4)),
          Number(Some(5)),
          Number(Some(6)),
          Number(Some(7)),
          Number(Some(8)),
          Number(None))))  mustEqual Set(2,9)
    }


    "return the numbers missing from a unordered square that is missing two numbers" in {


      SudukoSolver.missingNumberFromSquareFinder(
        Square(List(
          Number(Some(8)),
          Number(None),
          Number(Some(7)),
          Number(Some(5)),
          Number(Some(4)),
          Number(Some(6)),
          Number(Some(3)),
          Number(None),
          Number(Some(2)))))  mustEqual Set(1,9)
    }

  }

  "missingNumbersFromColumn" must {

    "return a all numbera from a given 3 empty squares, and a column position" in {

      SudukoSolver.missingNumbersFromColumn(
        List(squareBuilderNone,squareBuilderNone,squareBuilderNone), 1) mustEqual Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
    }

    "return missing numbers from a given 3 squares, and a column position" in {

      SudukoSolver.missingNumbersFromColumn(
        List(Square(List(
          Number(Some(1)),
          Number(None),
          Number(None),
          Number(None),
          Number(None),
          Number(None),
          Number(None),
          Number(None),
          Number(None))),squareBuilderNone,squareBuilderNone),0) mustEqual Set(2,3,4,5,6,7,8,9)}
  }

  "missing number of row " must {

    "return missing numbers from a given 3 squares, and a column position" in {

      SudukoSolver.missingNumbersFromColumn(
        List(Square(List(
          Number(Some(1)),
          Number(None),
          Number(None),
          Number(None),
          Number(None),
          Number(None),
          Number(None),
          Number(None),
          Number(None))),

          Square(List(
            Number(Some(2)),
            Number(None),
            Number(None),
            Number(None),
            Number(None),
            Number(None),
            Number(None),
            Number(None),
            Number(None))),

          Square(List(
            Number(Some(3)),
            Number(None),
            Number(None),
            Number(None),
            Number(None),
            Number(None),
            Number(None),
            Number(None),
            Number(None)))

        ),0) mustEqual Set(4,5,6,7,8,9)}


  }

  "venn diagram for cell" must {

    val pan :Grid = Grid(List(squareBuilderWithOneMissing(0,5),squareBuilder(3),squareBuilder(6),squareBuilder(1),squareBuilderNone,squareBuilderNone,squareBuilder(2)))

    "return the missing number from a grid that contains the rows neccicary to attain that number" in  {

      
      SudukoSolver.vennDiagramForCell(pan,0,4) mustEqual Set(5)

  }

    "Row set " in {

      SudukoSolver.missingNumbersFromRow(pan.squares.take(3),1) mustEqual Set(5)
    }

    "column Set" in {

      SudukoSolver.missingNumbersFromColumn(List(pan.squares(0),pan.squares(3),pan.squares(6)),1) mustEqual Set(5)
    }

  }
}

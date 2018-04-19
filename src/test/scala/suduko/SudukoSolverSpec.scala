package suduko

import org.scalatest._
import suduko.SudukoSolver._

class SudukoSolverSpec extends WordSpec with MustMatchers{

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


      SudukoSolver.missingNumberFromSquareFinder(
        Square(List(
          Number(Some(1)),
          Number(Some(2)),
          Number(Some(3)),
          Number(Some(4)),
          Number(Some(5)),
          Number(Some(6)),
          Number(Some(7)),
          Number(Some(8)),
          Number(Some(9)))))  mustEqual List()
    }

    "return all the numbers for a square that is empty " in {


      SudukoSolver.missingNumberFromSquareFinder(
        Square(List(
        Number(None),
        Number(None),
        Number(None),
        Number(None),
        Number(None),
        Number(None),
        Number(None),
        Number(None),
        Number(None))))  mustEqual List(1,2,3,4,5,6,7,8,9)
    }

    "return the number missing from a square that is missing one number " in {


      SudukoSolver.missingNumberFromSquareFinder(
        Square(List(
          Number(Some(1)),
          Number(Some(2)),
          Number(Some(3)),
          Number(Some(4)),
          Number(Some(5)),
          Number(Some(6)),
          Number(Some(7)),
          Number(Some(8)),
          Number(None))))  mustEqual List(9)
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
          Number(None))))  mustEqual List(2,9)
    }
  }
}

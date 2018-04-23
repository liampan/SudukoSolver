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

  def squareBuilderOnlyTopLeft(topLeftNum : Int) :Square ={
    Square(squareBuilderNone.numbers.updated(0,Number(Some(topLeftNum))))
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

    val pan :Grid = Grid(List(squareBuilderWithOneMissing(0,5),squareBuilder(3),squareBuilder(6),squareBuilder(1),squareBuilderNone,squareBuilderNone,squareBuilder(2),squareBuilderNone,squareBuilderNone))

    "return the missing number from a grid that contains the rows neccicary to attain that number" in  {


      SudukoSolver.vennDiagramForCell(pan,0,4) mustEqual Number(Some(5))
  }

    "return None from a grid that contains the rows that cant attain a definate number" in  {


      SudukoSolver.vennDiagramForCell(pan,4,3) mustEqual Number(None)
    }

    "return None from a row that is full and can not attain a number" in  {


      SudukoSolver.vennDiagramForCell(pan,0,0) mustEqual Number(None)
    }

  }
  "placer" must {

    val emptyGrid : Grid = Grid(List.range(0,8).map(x => squareBuilderNone))
    val topleftGrid : Grid = Grid(List(squareBuilderOnlyTopLeft(5))++List.range(1,8).map(x => squareBuilderNone))

    "given an empty grid place a number(some(5)) in the top left corner" in {

      SudukoSolver.placer(emptyGrid,0,0,Number(Some(5))) mustEqual topleftGrid
    }
  }

  "parser" must {

    val complete: Grid = Grid(List(squareBuilder(0), squareBuilder(3), squareBuilder(6),
                                    squareBuilder(1), squareBuilder(4), squareBuilder(7),
                                      squareBuilder(2), squareBuilder(5), squareBuilder(8)))

    "retrun a complete suduko when given a complete suduko" in {

      SudukoSolver.parser(complete) mustEqual complete
    }


    "replace the first missing number in (0,4) that is a 5" in {

      val oneMissing: Grid = Grid(List(squareBuilderWithOneMissing(0,4), squareBuilder(3), squareBuilder(6), squareBuilder(1), squareBuilder(4), squareBuilder(7), squareBuilder(2), squareBuilder(5), squareBuilder(8)))


      SudukoSolver.parser(oneMissing) mustEqual complete
    }

    "replace two missing numbers to complete a sudko" in  {

      val twoMissing : Grid = Grid(List(squareBuilderWithOneMissing(0,4), squareBuilder(3), squareBuilder(6), squareBuilder(1), squareBuilder(4), squareBuilder(7), squareBuilder(2), squareBuilder(5), squareBuilderWithOneMissing(8,3)))

      SudukoSolver.parser(twoMissing) mustEqual complete
    }
  }


  "ready input" must {

    "find all . and numbers in a string" in {

      SudukoSolver.readyInput(" 2 4 . | . . 1 | 5 . .") mustEqual List("2", "4", ".", ".", ".", "1", "5", ".", ".")
    }
  }

}

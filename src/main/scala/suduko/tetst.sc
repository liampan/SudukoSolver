import suduko.SudukoSolver.{Number, Square}

val SquareTL = Square(List(
  Number(Some(8)),
  Number(None),
  Number(Some(7)),
  Number(Some(5)),
  Number(Some(4)),
  Number(Some(6)),
  Number(Some(3)),
  Number(None),
  Number(Some(2))))



val x = List(1,2,3)
val y = List(1,2,4)

y.diff(x)


val squareSet = Set(1,2,3,5,4)
val columnSet = Set(2,3,5)
val rowSet = Set(8,2)

val intercept6 = squareSet.intersect(columnSet)

val intercept7 = squareSet.intersect(columnSet).intersect(rowSet)

//
//
//
//def left(list: Square ) :List[Number] ={List(list.numbers(0), list.numbers(3), list.numbers(6))}
//def centre(list: Square) :List[Number] ={ List(list.numbers(1),list.numbers(4),list.numbers(7))}
//def right(list: Square) :List[Number] ={ List(list.numbers(2), list.numbers(5),list.numbers(7))}
//
//def top[A](list: List[A]) :List[A] ={ list.take(3)}
//def middle[A](list: List[A]) :List[A] ={
//  list.slice(3,6)}
//def bottom[A](list: List[A]) :List[A] ={ list.takeRight(3)}
//
//
//
//
//
//SquareTL
//
//left(SquareTL)
//centre(SquareTL)
//right(SquareTL)
////
//top(SquareTL)
//middle(SquareTL)
//bottom(SquareTL)
//

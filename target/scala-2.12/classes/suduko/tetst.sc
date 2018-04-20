val SquareTL = List(1,2,3,4,5,6,7,8,9)











def left[A](list: List[A]) :List[A] ={List(list(0), list(3), list(6))}
def centre[A](list: List[A]) :List[A] ={ List(list(1),list(4),list(7))}
def right[A](list: List[A]) :List[A] ={ List(list(2), list(5),list(7))}

def top[A](list: List[A]) :List[A] ={ list.take(3)}
def middle[A](list: List[A]) :List[A] ={
  list.slice(3,6)}
def bottom[A](list: List[A]) :List[A] ={ list.takeRight(3)}


SquareTL

left(SquareTL)
centre(SquareTL)
right(SquareTL)

top(SquareTL)
middle(SquareTL)
bottom(SquareTL)


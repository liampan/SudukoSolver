package suduko

case class Square(numbers : List[Number]){


  def left :List[Number] ={List(this.numbers(0), this.numbers(3), this.numbers(6))}
  def centre :List[Number] ={ List(this.numbers(1),this.numbers(4),this.numbers(7))}
  def right :List[Number] ={ List(this.numbers(2), this.numbers(5),this.numbers(8))}

  def top :List[Number] ={ this.numbers.take(3)}
  def middle :List[Number] ={ this.numbers.slice(3,6)}
  def bottom :List[Number] ={ this.numbers.takeRight(3)}


  def render : String = {
    s"""
       |$top
       |$middle
       |$bottom
     """.stripMargin
  }


}

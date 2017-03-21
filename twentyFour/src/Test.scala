/**
  * Created by liuang on 2017/3/21.
  */
object Test {
  def main(args: Array[String]): Unit = {

  }
  def eval(str:String):Rational=str match {
    case Bracket(part1,expr,part2)=>eval(part1+eval(expr)+part2)
    case Add(expr1,expr2)=>eval(expr1)+eval(expr2)
    case Subtract(expr1,expr2)=>eval(expr1)-eval(expr2)
    case Multiply(expr1,expr2)=>eval(expr1)*eval(expr2)
    case Divide(expr1,expr2)=>eval(expr1)/eval(expr2)
    case _=>{
      new Rational(str.trim toInt,1)}
  }

}

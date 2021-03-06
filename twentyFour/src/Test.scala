/**
  * Created by liuang on 2017/3/21.
  */
object Test {
  val templates = List(
    "N*N-N+N",
    "(N-N)*N*N",
    "N*N+N*N",
    "(N+N)*N*N",
    "N*N*N*N",
    "(N+N*N)*N",
    "(N*N-N)*N",
    "N*N+N+N",
    "(N/N-N)*N",
    "(N-(N-N))*N",
    "N-(N-N-N)",
    "N+N-(N-N)",
    "N*(N/N-N)",
    "(N-N*N)*N",
    "N*(N-N)+N",
    "N+N+N/N",
    "(N-N)*(N-N)",
    "N+N*N/N",
    "N*N/(N-N)",
    "(N+N)*(N+N)",
    "(N-N)*N/N",
    "N+(N+N)/N",
    "N*N/(N+N)",
    "(N+N)*N/N",
    "(N*N+N)*N",
    "(N*N-N)/N",
    "(N/N+N)*N",
    "N*N/N/N",
    "N+N+N-N",
    "N-(N-N)+N",
    "N/(N-N/N)",
    "N+(N-N)*N",
    "(N+N+N)*N",
    "N+N*N-N",
    "N*N-N/N",
    "(N+N)*N-N",
    "(N+N)*(N-N)",
    "(N-N/N)*N",
    "N*(N+N)+N",
    "N*N+N/N",
    "N*N/N-N",
    "(N+N/N)*N",
    "N*N*N/N",
    "(N+N*N)/N",
    "N+N*N+N",
    "N-(N-N)*N",
    "(N-(N+N))*N",
    "N*N-N-N",
    "N+N/N+N",
    "(N-N)*N-N",
    "(N+N)/N+N",
    "N*N+N-N",
    "N/N+N+N",
    "N*N*N-N",
    "(N*N+N)/N",
    "N+N+N*N",
    "N*(N-N)/N",
    "N/N*N+N",
    "N+N*N*N",
    "N+N+N+N",
    "N*N/(N*N)",
    "N+(N+N)*N",
    "(N-N)*N+N",
    "(N+N+N)/N",
    "(N+N)*N+N",
    "N*N*N+N",
    "N*N-(N-N)",
    "N*N-(N+N)",
    "(N-N-N)*N",
    "N*N/N+N",
    "(N+N-N)*N",
    "N/(N/N-N)",
    "N*N-N*N"
  )

  def main(args: Array[String]): Unit = {
    val list = List(5)
    val vs = list map { v => (v.toDouble, null, null, null) }
    vs match {
      case x :: Nil => println(x._1 - 24)
      case _ => println("no")
    }
    //    for{x@(v1,_,_,_)<-vs;val xs=vs diff List(x)
    //        y@(v2,_,_,_)<-xs;val ys=xs diff List(y)
    //    }{
    //      println(x+" "+y)
    //    }
  }

  def eval(str:String):Rational=str match {
    case Bracket(part1,expr,part2)=>eval(part1+eval(expr)+part2)
    case Add(expr1,expr2)=>eval(expr1)+eval(expr2)
    case Subtract(expr1,expr2)=>eval(expr1)-eval(expr2)
    case Multiply(expr1,expr2)=>eval(expr1)*eval(expr2)
    case Divide(expr1,expr2)=>eval(expr1)/eval(expr2)
    case "" => new Rational(0, 1)
    case Rational(expr1, expr2) => new Rational(expr1 toInt, expr2 toInt)
    case _=>{
      new Rational(str.trim toInt,1)}
  }

  def solve(vs: List[Int], n: Int = 24): Unit = {
    def isZero(d: Double) = math.abs(d) < 0.00001

    def toStr(any: Any): String = any match {
      case (v: Double, null, null, null) => v.toInt.toString
      case (_, v1: (Double, Any, Any, Any), v2: (Double, Any, Any, Any), op) =>
        if (op == '-' && (v2._4 == '+' || v2._4 == '-'))
          "%s%c(%s)".format(toStr(v1), op, toStr(v2))
        else if (op == '/') {
          val s1 = if (v1._4 == '+' || v1._4 == '-') "(" + toStr(v1) + ")" else toStr(v1)
          val s2 = if (v2._4 == null) toStr(v2) else "(" + toStr(v2) + ")"
          s1 + op + s2
        }
        else if (op == '*') {
          val s1 = if (v1._4 == '+' || v1._4 == '-') "(" + toStr(v1) + ")" else toStr(v1)
          val s2 = if (v2._4 == '+' || v2._4 == '-') "(" + toStr(v2) + ")" else toStr(v2)
          s1 + op + s2
        }
        else toStr(v1) + op + toStr(v2)
    }

    val buf = collection.mutable.ListBuffer[String]()

    def solve0(xs: List[(Double, Any, Any, Any)]): Unit = xs match {
      case x :: Nil => if (isZero(x._1 - n) && !buf.contains(toStr(x))) {
        buf += toStr(x); println(buf.last)
      }
      case _ => for {x@(v1, _, _, _) <- xs; val ys = xs diff List(x)
                     y@(v2, _, _, _) <- ys; val rs = ys diff List(y)
      } {
        solve0((v1 + v2, x, y, '+') :: rs)
        solve0((v1 - v2, x, y, '-') :: rs)
        solve0((v1 * v2, x, y, '*') :: rs)
        if (!isZero(v2)) solve0((v1 / v2, x, y, '/') :: rs)
      }
    }

    solve0(vs map { v => (v.toDouble, null, null, null) })
  }

  def calculate(template: String, numbers: List[Int]) = {
    val values = template.split('N')
    var expression = ""
    for (i <- 0 to 3) expression = expression + values(i) + numbers(i)
    if (values.length == 5) expression = expression + values(4)
    (expression, template, eval(expression))
  }

  def cal24(input: List[Int]) = {
    var found = false
    for (template <- templates; list <- input.permutations) {
      try {
        val (expression, tp, result) = calculate(template, list)
        if (result.numer == 24 && result.demon == 1) {
          println(input + ":" + tp + ":" + expression)
          found = true
        }
      } catch {
        case e: Throwable =>
      }
    }
    if (!found) {
      println(input + ":" + "no result")
    }
  }

  def cal24once(input: List[Int]) = {
    var found = false
    for (template <- templates; list <- input.permutations if (!found)) {
      try {
        val (expression, tp, result) = calculate(template, list)
        if (result.numer == 24 && result.demon == 1) {
          println(input + ":" + tp + ":" + expression)
          found = true
        }
      } catch {
        case e: Throwable =>
      }
    }
    if (!found) {
      println(input + ":" + "no result")
    }
  }
}

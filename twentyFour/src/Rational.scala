/**
  * Created by liuang on 2017/3/21.
  */
class Rational (n:Int,d:Int){
 require(d!=0)
  val numer=n/g
  val demon=d/g
  private val g = gcd(n.abs, d.abs)

  override def toString: String = numer + "\\" + demon
  def +(that:Rational)=
    new Rational(numer*that.demon+that.numer*demon,demon*that.demon)

  def -(that:Rational)=
    new Rational(numer*that.demon-that.numer*demon,demon*that.demon)

  def *(that:Rational)=
    new Rational(numer*that.numer,demon*that.demon)

  def /(that:Rational)=
    new Rational(numer*that.demon,demon*that.numer)

  def this(n:Int)=this(n,1)

  private def gcd(a:Int,b:Int):Int=
    if(b==0) a else gcd(b,a%b)
}

object Rational extends {
  val op = "\\"
} with BinaryOp
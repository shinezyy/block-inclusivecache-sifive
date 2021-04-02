import scala.reflect.macros.Context
import scala.language.experimental.macros

object Play {
  def helloImpl(c: Context)(fmt: c.Expr[String], args: c.Expr[Int]*): c.Expr[Unit] = {
    import c.universe._
    println(fmt.tree)
  }

  def hello(fmt: String, args: Int*) = macro helloImpl
}
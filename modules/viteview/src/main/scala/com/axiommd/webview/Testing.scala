package com.axiommd.webview


import com.axiommd.webview.LaminarRender
import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

// enum Elements[T](i: intFlag, d: doubleFlag. s: stringFlag)
// case intFlag: Option[Int] = None,
// case doubleFlag: Option[Double]= None, 



// sealed abstract class flags (val intFlag: Int ) extends scala.reflect Enum


case class MyFlags(
  intFlag: Option[Int] = None,
  doubleFlag: Option[Double] = None,
  stringFlag: Option[String] = None
)

trait ToElement[T]:
  def element (x:T): Element

object module1 :
  given ToElement[Int] with
    def element(x:Int): Element  =  div(s"Hello Int $x")

  given ToElement[Double] with
      def element(x:Double) : Element = div(s"Hello Double $x" )  


object module2:
  given ToElement[Int] with
    def element(x:Int) : Element = div(s"Hello2 $x")

  given ToElement[Double] with
      def element(x:Double) : Element = div(s"Hello2 Double $x" )   

object module3:
    val numbers :Var[List[Int]] = Var(List(1,2,3))
    given ToElement[Int] with 
        def element(x:Int) : Element =
            table(tbody(children <--numbers.signal.map(x => Seq(tr(td(s"$x")),tr(s"$x"),tr(s"$x")))))
    
object RenderUsingFlags:
        given (using ToElement[Int], ToElement[Double]): ToElement[MyFlags] with
            def element(flags: MyFlags): Element =
              table(
                tbody(
                  tr(
                    flags.intFlag.map(i => td(f(i))),
                    flags.doubleFlag.map(d => tr(f(d))),
                    flags.stringFlag.map(s => tr(s"String: $s"))
                  )

                  
                )
              )

     

def f[T](x:T)(using mytostr : ToElement[T]) :Element =  
  mytostr.element(x)
  



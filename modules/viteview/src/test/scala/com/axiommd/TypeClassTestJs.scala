package com.axiommd

import org.scalatest._,  wordspec._, matchers._
import com.axiommd.webview.LaminarRender
import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}


trait ToElement[T]:
  def toElement (x:T): Element

object module1 :
  given ToElement[Int] with
    def toElement(x:Int): Element  =  div(s"Hello Int $x")

  given ToElement[Double] with
      def toElement(x:Double) : Element = div("Hello Double $x" )  


object module2:
  given ToElement[Int] with
    def toElement(x:Int) : Element = div("Hello2 $x")

  given ToElement[Double] with
      def toElement(x:Double) : Element = div("Hello2 Double $x" )   



def f[T](x:T)(using mytostr : ToElement[T]) :Element =  
  mytostr.toElement(x)
  

class TypeClassTestJs extends AnyWordSpec with should.Matchers{
  "this" should {
    "work" in {
      // import module2.given
      // val result = f(5).ref.textContent
      // info(s"$result")
      true should be(true)
    }
  }
}

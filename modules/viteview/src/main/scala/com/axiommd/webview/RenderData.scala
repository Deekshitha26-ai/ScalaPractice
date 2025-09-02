package com.axiommd.webview

import com.raquo.laminar.api.L.*

object RenderData:

  private object Icons8:
    val int        = "https://img.icons8.com/keek/100/i.png"
    val nullv      = "https://img.icons8.com/keek/100/i.png"
    val double     = "https://img.icons8.com/dusk/64/d.png"
    val string     = "https://img.icons8.com/dusk/64/s.png"
    val element    = "https://img.icons8.com/ios/50/puzzle.png"
  
  trait Render[-T]:
    def el(x: T): Element

  object Render:
    inline def apply[T](using r: Render[T]): Render[T] = r

   
    given Render[String]  with
      def el(x: String): Element  = span(x)
    given Render[Int]     with
      def el(x: Int): Element     = span(x.toString)
    given Render[Double]  with
      def el(x: Double): Element  = span(x.toString)
    given Render[Boolean] with
      def el(x: Boolean): Element = span(x.toString)

    
    given [A](using r: Render[A]): Render[Option[A]] with
      def el(x: Option[A]): Element = x.map(r.el).getOrElse(i("None"))

    
    given [A](using r: Render[A]): Render[Iterable[A]] with
      def el(xs: Iterable[A]): Element = ul(xs.toSeq.map(a => li(r.el(a))))

    
    given Render[Product] with
      def el(p: Product): Element =
        table(
          styleAttr := tableStyle,
          thead(
            tr(
              th(styleAttr := thStyle + " text-align:center; width:2.5rem;", "Icon"),
              th(styleAttr := thStyle, "Type"),
              th(styleAttr := thStyle, "Value")
            )
          ),
          tbody(
            p.productIterator
              .map(rowFor)                     // compute icon / type / rendered value
              .toSeq
              .map { r =>
                tr(
                  td(styleAttr := tdIconStyle, r.icon),
                  td(styleAttr := tdStyle, r.typeName),
                  td(styleAttr := tdStyle, r.value)
                )
              }
          )
        )

  
  def render[T](x: T)(using R: Render[T]): Element = R.el(x)

  private val tableStyle  = "border-collapse:collapse; margin:0.5rem 0;"
  private val thStyle     = "border:1px solid #000307ff; padding:6px 10px; background:#f8fafc; text-align:left; font-weight:600;"
  private val tdStyle     = "border:1px solid #000205ff; padding:6px 10px;"
  private val tdIconStyle = tdStyle + " text-align:center; font-size:18px;"

  private case class Row(icon: Element, typeName: String, value: Element)
  private def iconImg(url: String, label: String): Element =
    img(
      src := url,
      alt := label,
      styleAttr := "width:20px; height:20px; display:block; margin:0 auto;"
    )
  private def renderData(x: Any): Element = x match
    case null            => i("null")
    case e: Element      => e
    case s: String       => summon[Render[String]].el(s)
    case i: Int          => summon[Render[Int]].el(i)
    case d: Double       => summon[Render[Double]].el(d)
    


  private def rowFor(x:Any): Row = x match
    case null            => Row(iconImg(Icons8.nullv, "Null"), "Null", renderData(null))
    case e: Element      => Row(iconImg(Icons8.int, "Element"), "Element", e)
    case s: String       => Row(iconImg(Icons8.string, "String"), "String", renderData(s))
    case i: Int          => Row(iconImg(Icons8.int, "Int"), "Int", renderData(i))
    case d: Double       => Row(iconImg(Icons8.double, "Double"), "Double", renderData(d))


    case o: Option[?] =>
      o match
        case Some(i: Int)     => Row(iconImg(Icons8.int,    "Option[Int]"),    "Option[Int]",    renderData(i))
        case Some(d: Double)  => Row(iconImg(Icons8.double, "Option[Double]"), "Option[Double]", renderData(d))
        case Some(s: String)  => Row(iconImg(Icons8.string, "Option[String]"), "Option[String]", renderData(s))
        case Some(e: Element) => Row(iconImg(Icons8.element,"Option[Element]"),"Option[Element]", e)
        case Some(null)       => Row(iconImg(Icons8.nullv,  "Option[Null]"),   "Option[Null]",   i("null"))
        case None             => Row(iconImg(Icons8.element,"Option"),         "Option[?]",      i("None"))
        // guard against unexpected types so we never MatchError:
        case Some(other)      => Row(iconImg(Icons8.element,"Option"),         s"Option[${other.getClass.getSimpleName}]", span(other.toString))

    // (optional) if a nested case class ever appears, render it as a nested table
    case p: Product =>
      Row(iconImg(Icons8.element, "Product"), "Product", summon[Render[Product]].el(p))

    // final safety net so pattern match is total
    case other =>
      Row(iconImg(Icons8.element, "Other"), other.getClass.getSimpleName, span(other.toString))
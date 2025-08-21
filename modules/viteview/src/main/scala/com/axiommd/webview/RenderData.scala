package com.axiommd.webview

import com.raquo.laminar.api.L.{*, given}

object RenderData:

  
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
          styleAttr := "border-collapse:collapse; margin:0.5rem 0;",
          tbody(
            p.productElementNames.zip(p.productIterator).map { case (name, v) =>
              tr(
                th(
                  styleAttr := "border:1px solid #d1d5db; padding:6px 10px; background:#f8fafc; text-align:left; font-weight:600;",
                  name
                ),
                td(
                  styleAttr := "border:1px solid #d1d5db; padding:6px 10px;",
                  renderData(v)
                )
              )
            }.toSeq
          )
        )

  
  def render[T](x: T)(using Render[T]): Element =
  summon[Render[T]].el(x)

  
  private def renderData(x: Any): Element = x match
    case null            => i("null")
    case e: Element      => e
    case s: String       => summon[Render[String]].el(s)
    case i: Int          => summon[Render[Int]].el(i)
    case d: Double       => summon[Render[Double]].el(d)
    case b: Boolean      => summon[Render[Boolean]].el(b)
    case o: Option[?]    => o.fold(i("None"))(renderData)
    case it: Iterable[?] => ul(it.toSeq.map(v => li(renderData(v))))
    case p: Product      => summon[Render[Product]].el(p)
    case other           => span(other.toString)

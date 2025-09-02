package com.axiommd.webview

import com.raquo.laminar.api.L as L
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom

import L.*


sealed trait Status(_url: String):
  lazy val url = _url

final case class Completed()        extends Status("https://img.icons8.com/material-rounded/24/40C057/checked-checkbox.png")
final case class Draft(count: Int)  extends Status("https://img.icons8.com/ios-filled/50/FAB005/create-new.png")
final case class Urgent(count: Int) extends Status("https://img.icons8.com/fluency-systems-filled/48/FA5252/leave.png")

final case class Flags(
  completed: Option[Completed],
  draft:     Option[Draft],
  urgent:    Option[Urgent]
)

// Typeclass: map T -> <td> 
trait ToCell[T]:
  def tdElement(x: T): ReactiveHtmlElement[dom.HTMLTableCellElement]


def tablecell[T](data: T)(using tocell: ToCell[T]): ReactiveHtmlElement[dom.HTMLTableCellElement] =
  tocell.tdElement(data)

//  Base cell renderers 
given ToCell[Draft] with
  def tdElement(s: Draft) =
    L.td(
      div(
        cls := "cell",
        img(src := s.url, alt := "Draft"),
        div(s"x${s.count}")
      )
    )

given ToCell[Urgent] with
  def tdElement(s: Urgent) =
    L.td(
      div(
        cls := "cell",
        img(src := s.url, alt := "Urgent"),
        div(s"x${s.count}")
      )
    )

given ToCell[Completed] with
  def tdElement(s: Completed) =
    L.td(
      div(
        cls := "cell",
        img(src := s.url, alt := "Completed")
      )
    )

given [T](using tc: ToCell[T]): ToCell[Option[T]] with
  def tdElement(opt: Option[T]) = opt match
    case Some(v) => tc.tdElement(v)
    case None    => L.td("_")

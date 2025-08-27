package com.axiommd.webview

import org.scalajs.dom
import com.raquo.laminar.api.L.{*}
import com.raquo.laminar.nodes.ReactiveHtmlElement

sealed trait Status(_url:String) : 
  lazy val  url = _url

case class Completed() extends Status ("https://img.icons8.com/material-rounded/24/40C057/checked-checkbox.png")
case class Draft(count:Int) extends Status("https://img.icons8.com/ios-filled/50/FAB005/create-new.png")
case class Urgent(count:Int) extends Status("https://img.icons8.com/fluency-systems-filled/48/FA5252/leave.png")



trait ToCell[T]:
  def tdElement (x:T): ReactiveHtmlElement[dom.HTMLTableCellElement] //I found this more specific type by jumping around  the source code of laminar (ctrl + click), as well as reviewing compiler errors when I got the wrong type

/* Given instance for Option[T].  by using givens for T, you can now deal with Option[T] */
given [T](using tc: ToCell[T]): ToCell[Option[T]] with
  def tdElement(opt: Option[T]) = opt match
    case Some(value) => tc.tdElement(value)
    case None        => td("_")
  extension(optT: Option[T]) def optToTd = tdElement(optT) //extension method for convenience

given ToCell[Draft] with
  def tdElement (s:Draft) =   td(div(cls := "cell",img(src:=s.url), alt:= "Draft",div(s"x${s.count}")))

given ToCell[Urgent] with
  def tdElement (s:Urgent) =   td(div(cls := "cell",img(src:=s.url), alt:= "Urgent",div(s"x${s.count}")))

given ToCell[Completed] with
  def tdElement (s:Completed) =   td(div(cls := "cell",img(src:=s.url), alt:= "Completed"))

given ToCell[Flags] with
  def tdElement (s:Flags) =   td(tableElement(s))
  // render table row tag from flags
  private def tableRow(flags:Flags) = tr(flags.completed.optToTd, flags.draft.optToTd, flags.urgent.optToTd )
  // render table from flags
  private def tableElement(flags:Flags) = table(tableRow(flags) )

// Data Model for flags
case class Flags(completed: Option[Completed],draft:Option[Draft],urgent: Option[Urgent])



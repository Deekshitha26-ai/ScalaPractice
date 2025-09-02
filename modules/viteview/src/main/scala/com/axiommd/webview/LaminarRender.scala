package com.axiommd.webview
import com.raquo.laminar.api.L.*

object LaminarRender:
  def apply(): Element =
    val rows = List(
      "Task 1" -> Flags(Some(Completed()), Some(Draft(3)), None),
      "Task 2" -> Flags(None,               Some(Draft(1)), Some(Urgent(2)))
    )
    div(
      h2("Rendering data Into elements"),
      RenderUsingFlags.labeledFlagsTable(rows)
    )

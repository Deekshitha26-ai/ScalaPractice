package com.axiommd.webview

import com.raquo.laminar.api.L as L

import L.*

object RenderUsingFlags:

  private def header(cols: String*): HtmlElement =
    thead(tr(cols.map(th(_))*))

  // One row for a Flags value
  def flagsRow(f: Flags)(using
      ToCell[Option[Completed]],
      ToCell[Option[Draft]],
      ToCell[Option[Urgent]]
  ): HtmlElement =
    tr(
      tablecell(f.completed),
      tablecell(f.draft),
      tablecell(f.urgent)
    )

  // Single Flags -> table
  def flagsTable(f: Flags)(using
      ToCell[Option[Completed]],
      ToCell[Option[Draft]],
      ToCell[Option[Urgent]]
  ): HtmlElement =
    table(
      cls := "flags-table",
      header("Completed", "Draft", "Urgent"),
      tbody(flagsRow(f))
    )

  // Many Flags -> table
  def flagsTable[C[X] <: Iterable[X]](rows: C[Flags])(using
      ToCell[Option[Completed]],
      ToCell[Option[Draft]],
      ToCell[Option[Urgent]]
  ): HtmlElement =
    table(
      cls := "flags-table",
      header("Completed", "Draft", "Urgent"),
      tbody(rows.toSeq.map(flagsRow)*)
    )

  // Labeled rows -> table
  def labeledFlagsTable[C[X] <: Iterable[X]](rows: C[(String, Flags)])(using
      ToCell[Option[Completed]],
      ToCell[Option[Draft]],
      ToCell[Option[Urgent]]
  ): HtmlElement =
    table(
      cls := "flags-table",
      header("Task", "Completed", "Draft", "Urgent"),
      tbody(
        rows.toSeq.map { case (label, f) =>
          tr(
            L.td(label),                    
            tablecell(f.completed),
            tablecell(f.draft),
            tablecell(f.urgent)
          )
        }*
      )
    )

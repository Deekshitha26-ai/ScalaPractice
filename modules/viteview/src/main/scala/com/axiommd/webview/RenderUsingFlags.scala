package com.axiommd.webview
import com.raquo.laminar.api.L.{HtmlElement, given, *}



object RenderUsingFlags:

    object Render:

        trait ToElem[A]:
            def elem(a: A): Element

        // single Flags -> table (1 row)
        given flagsToElem: ToElem[Flags] with
            def elem(f: Flags): Element =
            table(
                cls := "flags-table",
                thead(tr(th("Completed"), th("Draft"), th("Urgent"))),
                tbody(
                tr(
                    summon[ToCell[Option[Completed]]].tdElement(f.completed),
                    summon[ToCell[Option[Draft]]].tdElement(f.draft),
                    summon[ToCell[Option[Urgent]]].tdElement(f.urgent)
                )
                )
            )

        // Iterable[Flags] -> table (many rows)
        given toElemIterableFlags[C[X] <: Iterable[X]]: ToElem[C[Flags]] with
            def elem(rows: C[Flags]): Element =
            table(
                cls := "flags-table",
                thead(tr(th("Completed"), th("Draft"), th("Urgent"))),
                tbody(
                rows.toSeq.map { f =>
                    tr(
                    summon[ToCell[Option[Completed]]].tdElement(f.completed),
                    summon[ToCell[Option[Draft]]].tdElement(f.draft),
                    summon[ToCell[Option[Urgent]]].tdElement(f.urgent)
                    )
                }*
                )
            )

        
        given toElemIterableLabeledFlags[C[X] <: Iterable[X]]: ToElem[C[(String, Flags)]] with
            def elem(rows: C[(String, Flags)]): Element =
            table(
                cls := "flags-table",
                thead(tr(th("Task"), th("Completed"), th("Draft"), th("Urgent"))),
                tbody(
                rows.toSeq.map { case (label, f) =>
                    tr(
                    td(label),
                    summon[ToCell[Option[Completed]]].tdElement(f.completed),
                    summon[ToCell[Option[Draft]]].tdElement(f.draft),
                    summon[ToCell[Option[Urgent]]].tdElement(f.urgent)
                    )
                }*
                )
            )


    def render[A](a: A)(using r: Render.ToElem[A]): Element = r.elem(a)

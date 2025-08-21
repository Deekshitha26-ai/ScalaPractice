package com.axiommd.webview


import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits.global

object LaminarRender :
  def consoleOut(msg: String): Unit = {
    dom.console.log(s"%c $msg","background: #222; color: #bada55")
  }

  def apply():Element = 

    // ModelFetch.fetchPatients.foreach{ p => 
    //   consoleOut(s"Patient: $p")
    // }
    // import com.axiommd.webview.module1.given         
    // import com.axiommd.webview.RenderUsingFlags.given

    // val flags = MyFlags(intFlag = Some(5), doubleFlag = Some(2.5), stringFlag = Some("hi"))
    // f(flags)

    // div("hello world Deekshita!!!!!!!!!!!!!!!!!!")    
    // div ("cell 2")
    // div("cell 3")

    import RenderData.Render.given   

    val flags = MyFlags(intFlag = Some(5), doubleFlag = Some(2.5), stringFlag = Some("hi"))

    
    div(
      h2("Rendering data Into elements"),
      RenderData.render(flags)   
    )
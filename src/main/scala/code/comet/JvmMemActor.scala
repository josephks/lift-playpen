package code.comet

import code.lib.SimpFactory

         import xml.{Text, NodeSeq}
import net.liftweb.common.{Logger, Full}
import net.liftweb.http.{CometActor, S, DispatchSnippet, SHtml}
import net.liftweb.util.{Schedule, Helpers}

import net.liftweb.http.js.JsCmds.{Replace, SetHtml, Noop}
import net.liftweb.http.js.JsExp

/**
 * Created with IntelliJ IDEA.
 * User: jks
 * Date: 4/27/12
 * Time: 4:14 PM
 * To change this template use File | Settings | File Templates.
 */

class JvmMemActor    extends CometActor with Logger{

  private val nbsp: Char = 160
  val DATE_FORMAT = "EEE"+nbsp+"MMM"+nbsp+"dd"+nbsp+"HH:mm:ss"
  val throbber = <img src="/images/ajax-loader.gif" style="margin-bottom: 0px; margin-left: 5px" id="ajax-loader" alt="" />

  class UpdateableSpan(val uuid: String){
     def getSpan(node: scala.xml.Node): scala.xml.Elem = <span id={ uuid } >{ node }</span>
    def getSpan: scala.xml.Elem = getSpan(Text("..."))//return a placeholder span to be inserted into html on doc creation
    def setHtml(node: scala.xml.Node)  = partialUpdate(SetHtml(uuid, node))
    def setHtml(text: String)  = partialUpdate(SetHtml(uuid, <div>{ text }</div>))
  }

  val memops = List("freemem" , "usedmem", "totalmem", "maxmem") //, "memdate")

    class FreeTotalMax(){
      val date = new java.util.Date
      def free = Runtime.getRuntime().freeMemory()
      def total = Runtime.getRuntime().totalMemory()
      def max =  Runtime.getRuntime().maxMemory()
      def used = total - free
    }
  private def formatNum(num: Long)={
    java.text.NumberFormat.getInstance().format(num)
  }
  private def joinNodeSeqs( seq: Seq[NodeSeq]) : NodeSeq = {
    val nb = new scala.xml.NodeBuffer
    seq.foreach( nb ++= _)
    nb
  }
  private def toElem(nodeseq: scala.xml.NodeSeq)= {
    if (nodeseq.isInstanceOf[scala.xml.Elem]){
      nodeseq.asInstanceOf[scala.xml.Elem]
    }else if (nodeseq.size == 1 && nodeseq(0).isInstanceOf[scala.xml.Elem] ){
      nodeseq(0).asInstanceOf[scala.xml.Elem]
    }else{
      <span>{ nodeseq }</span>
    }
  }

    private var updateables = List[{def doUpdate(ftm: FreeTotalMax)}]()


    def getSpanFor(op: String) = {
      val ans =
        new UpdateableSpan("s"+SimpFactory.inject[ SimpFactory.UniqueNumber].get) {
          def getText(ftm: FreeTotalMax) = {
            op match{
              case "memdate" =>  new java.text.SimpleDateFormat(DATE_FORMAT) format ftm.date
              case _ =>
                formatNum(op match{
                  case "freemem" => ftm.free
                  case  "usedmem" => ftm.used
                  case  "totalmem" => ftm.total
                  case  "maxmem" => ftm.max
                })
            }//op match
          }//getText
          def doUpdate(ftm: FreeTotalMax){
            setHtml(getText(new FreeTotalMax))
          }
        } //MemSpan
       updateables = ans :: updateables
      ans.getSpan
    }

    class FlotChart(origHtml: scala.xml.Elem){ //the top level, class="memchart"
                        import net.liftweb.widgets.flot._
        import code.lib.Implicits._
      var checkBoxSpanId: Option[String] = None
      val uuid = "flotspan"+SimpFactory.inject[ SimpFactory.UniqueNumber].get
      //html2 is original with checkboxes div replaced
      private val html2 = toElem( (".checkboxes" #> ((checkboxDiv: scala.xml.NodeSeq) ⇒ {
        val ans = new UpdateableSpan("cbx"+SimpFactory.inject[ SimpFactory.UniqueNumber].get)
        checkBoxSpanId = Some(ans.uuid)
        ans.getSpan(Text(""))
      })).apply(origHtml) )
      private val DEFAULT_INTERVALS = 100
      //html3 is html2, with the class of root possibly set to "flotchart"
      //flot_chart is the Elem that houses the actual chart
      private val (html3, flot_span, timepoints) = html2.searchForNodeWithAttrib( "class" -> "flotchart") match {
        case Some(chartnode) => (html2, //unchanged
                                 chartnode,
                                 (chartnode \ "@intervals").text match { case "" => DEFAULT_INTERVALS case x => x.toInt} )
        case None =>
          //If no child node is annotated with  class="flotchart" set it on the root
          val newroot = html2 % new scala.xml.UnprefixedAttribute("class", "flotchart", scala.xml.Null) //use top level
          (newroot, newroot, DEFAULT_INTERVALS)
      }
      //now use a css transform on html3 to create the final span
      val getSpan = (".flotchart" #> ((chartDiv: scala.xml.NodeSeq) ⇒ { chartDiv.asInstanceOf[scala.xml.Elem] % new scala.xml.UnprefixedAttribute("id", uuid, scala.xml.Null) })).apply(html3)

      val memspan = new UpdateableSpan(uuid) {
        private def getDataPoint(op: String, ftm: FreeTotalMax): (Double, Double) =
          (ftm.date.getTime,
           op match{
             case "freemem" => ftm.free
             case  "usedmem" => ftm.used
             case  "totalmem" => ftm.total
             case  "maxmem" => ftm.max
           })

        var data_lines: List[FlotSerie] = null //set in handleFirstDataPoint()
        var pointsDone = 0
        private def handleFirstDataPoint(ftm: FreeTotalMax) = { //on first datapoint render the chart
          data_lines = memops.map( op =>  new FlotSerie(){
              override val data = List( getDataPoint(op, ftm) )
            }
          )
          val flot_widget_rendered =  Flot.render(uuid, data_lines, new FlotOptions {
            override def xaxis = Full(
              new  FlotAxisOptions{
                override def mode = Full("time")
              })
          }, Flot.script(flot_span))

          partialUpdate(SetHtml(uuid, flot_widget_rendered ))
          //now create the checkboxes.  Can't be created until data_lines is created
          checkBoxSpanId match{
            case None =>
            case Some(cbx_id) =>
              partialUpdate(SetHtml(cbx_id, joinNodeSeqs( memops.zipWithIndex.map{case (name, idx) =>
                <label> {
                  SHtml.ajaxCheckbox (true, { (b: Boolean) =>
                    JsFlotSetOps(uuid, data_lines,
                             {  val array = Array.fill[List[ (String, JsExp)]](memops.size)(Nil)
                              array(idx) = List(("lines.lineWidth", if(b){ 2 }else{ 0 }))
                              array.toList
                            })  } ) }{ name} </label>  }) ))
          }//match
        } //handleFirstDataPoint(
        def doUpdate(ftm: FreeTotalMax) = {
          if (pointsDone == 0){
             handleFirstDataPoint(ftm)
          }else{
            val doPop = pointsDone >= timepoints  //If we have done timepoints worth of data already drop old datapoints
            //Update flot chart here
            partialUpdate( JsFlotAppendData( uuid, data_lines,  memops.map( op =>  getDataPoint(op, ftm) ),  doPop)  )
          }
          pointsDone += 1
        } //updateData
      }
      updateables = memspan :: updateables
    }


  def getMemChart: scala.xml.NodeSeq ⇒ scala.xml.NodeSeq = {
    (outerdiv: scala.xml.NodeSeq) ⇒ {
      new FlotChart( toElem(outerdiv)).getSpan
    }
  } // getMemChart

  def render = {
    debug("render starting")
    val cssSelA  = (".memchart" #> getMemChart )       &
      ".refreshbutton" #> SHtml.ajaxButton ( scala.xml.Text("Refresh All"), () => {
        doUpdate
        Noop
      })
    val cssSelB = (cssSelA /: ("memdate" :: memops).map( opname => ("."+opname) #> getSpanFor(opname) ) ){ _ & _ }
    // cssSelB.apply ( node )
    Schedule.perform(this, UpdateInfo, 100)
     cssSelB
  }
  def doUpdate(){
    debug("doUpdate starting")
    updateables.foreach( _.doUpdate(new FreeTotalMax) )
    Schedule.perform(this, UpdateInfo, 5000)
  }
   override def lowPriority : PartialFunction[Any, Unit] = {
    case UpdateInfo => {
      doUpdate()
    }
  }
}
      case object UpdateInfo
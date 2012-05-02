package code.comet

import code.lib.SimpFactory

         import xml.{Text, NodeSeq}
import net.liftweb.http.{CometActor, S, DispatchSnippet, SHtml}
import net.liftweb.util.{Schedule, Helpers}

import _root_.net.liftweb.util.Helpers._
import net.liftweb.http.js.JsCmds.{Replace, SetHtml, Noop}
import net.liftweb.http.js.JsExp
import net.liftweb.common.{Box, Logger, Full}

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

  class MyFlotSerie(val lbl: String, val _color: String) extends net.liftweb.widgets.flot.FlotSerie{
    override def label: Box[String] = Full(lbl)
    override def color: Box[Either[String, Int]] = Full(Left(_color) )
  }

  val memops = List("freemem" , "usedmem", "totalmem", "maxmem")
  val lineOptions = List( "green", "yellow", "blue", "black")

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
        private def getDataPoint(op: String, ftm: FreeTotalMax): (Double, Double) = {
          (ftm.date.getTime,
           op match{
             case "freemem" => ftm.free
             case  "usedmem" => ftm.used
             case  "totalmem" => ftm.total
             case  "maxmem" => ftm.max
           })
        }

        var data_lines: List[FlotSerie] = null //set in handleFirstDataPoint()
        var pointsDone = 0
        var show_toggle: Array[Boolean] = null  //corresponds to each FlotSerie

        private def handleFirstDataPoint(ftm: FreeTotalMax) = { //on first datapoint render the chart
          data_lines = memops.zip(lineOptions).map( {case (op, color) =>
            new MyFlotSerie(op, color){
              override val data = List( getDataPoint(op, ftm) )
            }
          })
          show_toggle = memops.map( _ =>  true).toArray

          //get series to be rendered, based on if their boolean is set in show_toggle
          def serieToRender = show_toggle.toList.zip(data_lines).filter{ case (b, _) => b }.map{ case (_, s) => s  }

          class MyFlotOptions extends FlotOptions {
            override def xaxis = Full(
              new  FlotAxisOptions{
                override def mode = Full("time")
              })
          }

          val flot_widget_rendered =  Flot.render(uuid, data_lines, new MyFlotOptions, Flot.script(flot_span))

          partialUpdate(SetHtml(uuid, flot_widget_rendered ))
          //now create the checkboxes.  Can't be created until data_lines is created

          val USE_CLIENT_SIDE = true //use client side javascript alternative

          val legend_checkbox_server_side =  <label> {SHtml.ajaxCheckbox (true, { (b: Boolean) =>
                     val newOptions = new MyFlotOptions{ override def legend = Full(new FlotLegendOptions{ override def show = Full(b)})}
                  //Doesn't work on its own. The options passed into renderFlotShow() are not actually used in its code
//                         Flot.renderFlotShow ( uuid,   serieToRender, newOptions, Noop)

                        net.liftweb.http.js.JsCmds.JsCrVar("options_"+uuid, newOptions.asJsObj) &
                         Flot.renderFlotShow ( uuid, null, null, Noop)
                  } ) }Legend </label>

           if (USE_CLIENT_SIDE){
                 val options_var_name = "options_"+uuid
                 partialUpdate(net.liftweb.http.js.JE.JsRaw(
                       """function onLgndClick(b){
                             """+options_var_name+""" = jQuery.extend( """+options_var_name+"""  , { legend: { show: b}}   )
                             """ + Flot.renderFlotShow ( uuid, null, null, Noop).toJsCmd + """
                 }"""
                 ).cmd)
           }

           val legend_checkbox_client_side =   <label > <input checked="checked" type="checkbox" onclick=" onLgndClick(this.checked)" />
                     Legend </label>

          checkBoxSpanId match{
            case None =>
            case Some(cbx_id) =>
              partialUpdate(SetHtml(cbx_id, joinNodeSeqs(
              List(if (USE_CLIENT_SIDE){ legend_checkbox_client_side }else{ legend_checkbox_server_side }) ++
                memops.zipWithIndex.map{case (name, idx) =>
                <label> {
                  SHtml.ajaxCheckbox (true, { (b: Boolean) =>
//                    JsFlotSetOps(uuid, data_lines,
//                             {  val array = Array.fill[List[ (String, JsExp)]](memops.size)(Nil)
//                              array(idx) = List(("lines.lineWidth", if(b){ 2 }else{ 0 }))
//                              array.toList
//                            })
                     show_toggle(idx) = b
                    //now rerender widget
                     net.liftweb.http.js.JsCmds.JsCrVar("datas_"+uuid, Flot.renderSeries(serieToRender, uuid)) &
                     Flot.renderFlotShow ( uuid,   serieToRender,
                           new FlotOptions{}, Noop)
                  } ) }{ name} </label>  }) ))
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
    val cssSelA  = (".memchart" #> getMemChart )  &
          ".refreshseconds" #> SHtml.ajaxText("0", (str) => {
            update_interval = str.toInt
            this ! "update"
      })  &
      ".refreshbutton" #> SHtml.ajaxButton ( scala.xml.Text("Refresh Now"), () => {
        doUpdate
        Noop
      })
    val cssSelB = (cssSelA /: ("memdate" :: memops).map( opname => ("."+opname) #> getSpanFor(opname) ) ){ _ & _ }
    // cssSelB.apply ( node )
    Schedule.perform(this, "update", 100)
     cssSelB
  }
  def doUpdate(){
    trace("doUpdate starting")
    updateables.foreach( _.doUpdate(new FreeTotalMax) )
  }
  private var update_interval = 0 //in seconds

  override def lowPriority : PartialFunction[Any, Unit] = {
//    case UpdateInfo => {
//      doUpdate()
    case "update" =>
      doUpdate
      if (update_interval > 0)  {
        //todo: figure out way to do this only if there isn't already an update scheduled, or cancel that one
        if (false){
          //old: server side scheduling of next push:
          Schedule.schedule(this, "update", update_interval * 1000)
          //I just keep this around in case I need it for debugging purposes
        }else{
          //new: client side request for update, to avoid pushing over a bad connection
          val json_send_jscmd = //jsonSend(net.liftweb.http.js.JE.Num(666))
            jsonSend("update")
          trace("json send cmd: "+json_send_jscmd)

import net.liftweb.http.js.JsCmds._
          partialUpdate( After(update_interval seconds, json_send_jscmd   ))
          //partialUpdate( After(2500 millis,{jsonSend( net.liftweb.http.js.JE.Num(666)) }) )
          trace("sent after command")
        }
      }
  }

  override def  receiveJson = { //: PartialFunction[JValue, JsCmd] = {
    case jvalue =>
      trace("receiveJson(): jvalue: "+jvalue)
      this ! "update"
      net.liftweb.http.js.JsCmds.Noop
  }
  override def  autoIncludeJsonCode = true
}
    //  case object UpdateInfo

/*

try { destroy_F81793020218TX5GBD(); } catch (e) {}

jQuery('#'+"flotspan5").html("\u000a      <script src=\"/classpath/flot/jquery.flot.js\" type=\"text/javascript\"></script><script src=\"/classpath/flot/jquery.flot.navigate.js\" type=\"text/javascript\"></script>\u000a    <!--[if IE\u005d><script language=\"javascript\" type=\"text/javascript\" src=\"/classpath/flot/excanvas.js\"></script><![endif\u005d-->\u000a      <link type=\"text/css\" rel=\"stylesheet\" href=\"/classpath/flot/jquery.flot.css\" />\u000a    ");
// <![CDATA[
var data_flotspan5_1 = [[1.335922658888E12, 1.31983E8]
]
;
var data_flotspan5_2 = [[1.335922658888E12, 2.1502312E7]
]
;
var data_flotspan5_3 = [[1.335922658888E12, 1.53485312E8]
]
;
var data_flotspan5_4 = [[1.335922658888E12, 1.854865408E9]
]
;
var datas_flotspan5 = [{"data": data_flotspan5_1}, {"data": data_flotspan5_2}, {"data": data_flotspan5_3}, {"data": data_flotspan5_4}]
;
var options_flotspan5 = {"xaxis": {"mode": "time"}};
function flot_plot_flotspan5() {
    jQuery('#'+"flotspan5").addClass("flot_lww");
jQuery('#'+"flotspan5").show();

var plot_flotspan5 = jQuery.plot(jQuery("#flotspan5"), datas_flotspan5, options_flotspan5);


    }

jQuery(document).ready(function() {flot_plot_flotspan5();;});
// ]]>
;;
jQuery('#'+"cbx6").html("<label> <input checked=\"checked\" type=\"checkbox\" onclick=\"liftAjax.lift_ajaxHandler(\u0027F81793020228Z0VYQ1=\u0027 + this.checked, null, null, null)\" />freemem </label><label> <input checked=\"checked\" type=\"checkbox\" onclick=\"liftAjax.lift_ajaxHandler(\u0027F81793020230V42XAL=\u0027 + this.checked, null, null, null)\" />usedmem </label><label> <input checked=\"checked\" type=\"checkbox\" onclick=\"liftAjax.lift_ajaxHandler(\u0027F81793020232LMFHW2=\u0027 + this.checked, null, null, null)\" />totalmem </label><label> <input checked=\"checked\" type=\"checkbox\" onclick=\"liftAjax.lift_ajaxHandler(\u0027F817930202340LATDB=\u0027 + this.checked, null, null, null)\" />maxmem </label>");
jQuery('#'+"s4").html("<div>1,854,865,408</div>");
jQuery('#'+"s3").html("<div>153,485,312</div>");
jQuery('#'+"s2").html("<div>21,921,016</div>");
jQuery('#'+"s1").html("<div>131,564,296</div>");
jQuery('#'+"s0").html("<div>Tue\u00a0May\u00a001\u00a021:37:38</div>");
try { destroy_F81793020218TX5GBD = function() {}; } catch (e) {}
if (lift_toWatch['F81793020218TX5GBD'] !== undefined) lift_toWatch['F81793020218TX5GBD'] = '81793020241';
*/
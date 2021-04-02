package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util.DecoupledIO
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import scala.collection.mutable

object GTimer {
  def apply() = {
    val c = RegInit(0.U(64.W))
    c := c + 1.U
    c
  }
}


object JsonDump
{
  def dump[T <: Data](v: Vec[T], field_fmt: StringBuilder, fields: mutable.ListBuffer[Data]): Unit = {
    val fmts = mutable.ListBuffer.empty[String]

    val my_field_fmt = new StringBuilder
    v foreach {
      case bundle: Bundle =>
        my_field_fmt ++= "{ "
        dump(bundle, my_field_fmt, fields)
        my_field_fmt ++= " }"
        fmts += my_field_fmt.toString
        my_field_fmt.clear()
      case vec: Vec[_] =>
        my_field_fmt ++= "[ "
        dump(vec, my_field_fmt, fields)
        my_field_fmt ++= " ]"
        fmts += my_field_fmt.toString
        my_field_fmt.clear()
      case e =>
        my_field_fmt ++= """"0x%x""""
        fmts += my_field_fmt.toString
        fields += e
        my_field_fmt.clear()
    }

    field_fmt ++= fmts.mkString(", ")
  }

  def dump(r: Bundle, formatter: StringBuilder, fields: mutable.ListBuffer[Data]): Unit = {
    val field_formats = mutable.ListBuffer.empty[String]

    // Prettify ValidIO / DecoupledIO which has nested bits field
    val elements = mutable.ListMap.empty[String, Data]
    r.elements.foreach { case (name, data) =>
      if (name == "bits" && data.isInstanceOf[Bundle]) {
        elements ++= data.asInstanceOf[Bundle].elements
      }
      else {
        elements += name -> data
      }
    }

    elements foreach { case (name, data) =>
      val field_format = new StringBuilder(s""""$name": """)
      data match {
        case bundle: Bundle =>
          field_format ++= "{ "
          dump(bundle, field_format, fields)
          field_format ++= " }"
        case vec: Vec[_] =>
          field_format ++= "[ "
          dump(vec, field_format, fields)
          field_format ++= " ]"
        case _: BundleMap =>
          /* skip ((( */
        case _ =>
          field_format ++= """"0x%x""""
          fields += data
      }
      field_formats += field_format.toString
    }

    formatter ++= field_formats.mkString(", ")
  }

  def dump(params: InclusiveCacheParameters, r: Bundle, prefix_fmt: String, prefix_fields: Data*): Unit = {
    // println(s"handle ${r.className}")
    val (brace_l, brace_r) = if (prefix_fmt.length > 0) ("{ ", " }") else ("", "")

    val fmt = new StringBuilder(s""""classType": "${r.className}", """)
    val fields = mutable.ListBuffer.empty[Data]


    fmt ++= prefix_fmt
    fmt ++= brace_l
    fields ++= prefix_fields
    JsonDump.dump(r, fmt, fields)
    fmt ++= brace_r
    sanity_check(fmt.toString, fields.toList)
    DebugPrint(params, Printable.pack("{ " + fmt + " }\n", fields.toList:_*))
  }

  def dump(params: InclusiveCacheParameters, r: Bundle): Unit = {
    dump(params, r, "")
  }

  def sanity_check(fmt: String, fields: List[Data]): Unit = {
    val count = fmt.count(_ == '%')
    // println(s"fmt = {${fmt}}")
    // println(s"%_count: ${count}, fields: ${fields.size}")
    // val sinalClasses = fields.map(_.getClass.getName)
    // println(sinalClasses.mkString(", "))
    require(count == fields.size)
  }
}


trait HasJsonDump
{
  this: Bundle =>

  def dump(params: InclusiveCacheParameters, prefix_fmt: String, prefix_fields: Data*): Unit = {
    JsonDump.dump(params, this, prefix_fmt, prefix_fields:_*)
  }

  def dump(params: InclusiveCacheParameters): Unit = {
    this.dump(params, "")
  }
}


object DebugPrint {
  def apply(params: InclusiveCacheParameters, fmt: String, data: Bits*): Any =
    apply(params, Printable.pack(fmt, data:_*))

  def apply(params: InclusiveCacheParameters, pable: Printable): Any = {
    if (params.debug) {
      val commonInfo = p"[DEBUG][time=${GTimer()}] 9527: "
      printf(commonInfo + pable)
    }
  }
}

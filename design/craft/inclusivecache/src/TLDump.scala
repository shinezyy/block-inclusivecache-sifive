package sifive.blocks.inclusivecache

import Chisel._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

trait HasTLDump {

  implicit class TLDump(channel: TLChannel) {
    def dump(params: InclusiveCacheParameters): Unit = {
      JsonDump.dump(params, channel)
    }
  }
}

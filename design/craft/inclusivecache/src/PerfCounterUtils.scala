package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util.experimental.BoringUtils

object XSPerfAccumulate {
  def apply(params: InclusiveCacheParameters, perfName: String, perfCnt: UInt) = {
    if (params.enablePerf) {
      val logTimestamp = WireInit(0.U(64.W))
      val perfClean = WireInit(false.B)
      val perfDump = WireInit(false.B)
      BoringUtils.addSink(logTimestamp, "logTimestamp")
      BoringUtils.addSink(perfClean, "XSPERF_CLEAN")
      BoringUtils.addSink(perfDump, "XSPERF_DUMP")

      val counter = RegInit(0.U(64.W))
      val next_counter = counter + perfCnt
      counter := Mux(perfClean, 0.U, next_counter)

      when (perfDump) {
        DebugPrint(params, p"$perfName, $next_counter\n")
      }
    }
  }
}

object XSPerfHistogram {
  // instead of simply accumulating counters
  // this function draws a histogram
  def apply(params: InclusiveCacheParameters, perfName: String, perfCnt: UInt, enable: Bool, start: Int, stop: Int, step: Int) = {
    if (params.enablePerf) {
      val logTimestamp = WireInit(0.U(64.W))
      val perfClean = WireInit(false.B)
      val perfDump = WireInit(false.B)
      BoringUtils.addSink(logTimestamp, "logTimestamp")
      BoringUtils.addSink(perfClean, "XSPERF_CLEAN")
      BoringUtils.addSink(perfDump, "XSPERF_DUMP")

      // drop each perfCnt value into a bin
      val nBins = (stop - start) / step
      require(start >= 0)
      require(stop > start)
      require(nBins > 0)

      (0 until nBins) map { i =>
        val binRangeStart = start + i * step
        val binRangeStop = start + (i + 1) * step
        val inRange = perfCnt >= binRangeStart.U && perfCnt < binRangeStop.U

        // if perfCnt < start, it will go to the first bin
        val leftOutOfRange = perfCnt < start.U && i.U === 0.U
        // if perfCnt >= stop, it will go to the last bin
        val rightOutOfRange = perfCnt >= stop.U && i.U === (nBins - 1).U
        val inc = inRange || leftOutOfRange || rightOutOfRange

        val counter = RegInit(0.U(64.W))
        when (perfClean) {
          counter := 0.U
        } .elsewhen(enable && inc) {
          counter := counter + 1.U
        }

        when (perfDump) {
          DebugPrint(params, p"${perfName}_${binRangeStart}_${binRangeStop}, $counter\n")
        }
      }
    }
  }
}
object XSPerfMax {
  def apply(params: InclusiveCacheParameters, perfName: String, perfCnt: UInt, enable: Bool) = {
    if (params.enablePerf) {
      val logTimestamp = WireInit(0.U(64.W))
      val perfClean = WireInit(false.B)
      val perfDump = WireInit(false.B)
      BoringUtils.addSink(logTimestamp, "logTimestamp")
      BoringUtils.addSink(perfClean, "XSPERF_CLEAN")
      BoringUtils.addSink(perfDump, "XSPERF_DUMP")

      val max = RegInit(0.U(64.W))
      val next_max = Mux(enable && (perfCnt > max), perfCnt, max)
      max := Mux(perfClean, 0.U, next_max)

      when (perfDump) {
        DebugPrint(params, p"${perfName}_max, $next_max\n")
      }
    }
  }
}

object TransactionLatencyCounter
{
  // count the latency between start signal and stop signal
  // whenever stop signals comes, we create a latency sample
  def apply(start: Bool, stop: Bool): (Bool, UInt) = {
    assert (!(start && stop))
    val counter = RegInit(0.U(64.W))
    val next_counter = counter + 1.U
    counter := Mux(start || stop, 0.U, next_counter)
    (stop, next_counter)
  }
}

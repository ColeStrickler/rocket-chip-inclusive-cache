/*
 * Copyright 2019 SiFive, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You should have received a copy of LICENSE.Apache2 along with
 * this software. If not, you may obtain a copy at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._

import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import java.rmi.AccessException
import com.fasterxml.jackson.annotation.JsonProperty.Access
import midas.targetutils.SynthesizePrintf
import  freechips.rocketchip.interrupts._
import freechips.rocketchip.util.Annotated.interrupts

class CacheAccessCounterIO(num_banks: Int, num_cpus : Int, TLBundleParam : TLBundleParameters) extends Bundle {
    val TLIn = Input(Vec(num_banks, TLBundle(TLBundleParam)))
    val TLOut = Input(Vec(num_banks, TLBundle(TLBundleParam)))
    val interrupt = Output(Vec(num_cpus, Bool()))
}


class CacheAccessCounter(accCounterParams: CacheAccessCounterParams) (implicit p: Parameters)  extends LazyModule
{
  lazy val module = new CacheAccessCounterModule(this, accCounterParams)
}


case class CacheAccessCounterParams
(
    num_cpus : Int,
    tlBundleParam: TLBundleParameters
)


class CacheAccessCounterModule(
  val outer : CacheAccessCounter,
  val accCounterParams : CacheAccessCounterParams,
  ) extends LazyModuleImp(outer)
{
    val nBanks = p(BankedL2Key).nBanks
    val io = IO(new CacheAccessCounterIO(nBanks, accCounterParams.num_cpus, accCounterParams.tlBundleParam))


    


    /*
        Settings Registers
    */
    val memBase = p(ExtMem).get.master.base.U
    val countInstFetch = RegInit(true.B)
    val AccessCounterReset = RegInit(false.B)
    val EnableInterrupt = Seq.fill(accCounterParams.num_cpus)(RegInit(false.B))
    val wPeriod = 25 // for max 33.5ms period, F = 1GHz
    val w = wPeriod - 3 // it can count up to a transaction per 8 cycles when window size is set to max
    val periodLen = Reg(UInt(wPeriod.W))


    /*
        Per-CacheBank counters
    */
    val PerBankMissCounters =  Seq.fill(nBanks)(RegInit(VecInit(Seq.fill(accCounterParams.num_cpus)(0.U(64.W)))))
    val PerBankAccessCounters = Seq.fill(nBanks)(RegInit(VecInit(Seq.fill(accCounterParams.num_cpus)(0.U(64.W)))))
    // Per-CPU counters
    val MissCounters = Seq.fill(accCounterParams.num_cpus)(RegInit(0.U(64.W)))
    val AccessCounters = Seq.fill(accCounterParams.num_cpus)(RegInit(0.U(64.W)))
    // Per-CPU Regulation Budgets
    val CoreBudgets = Seq.fill(accCounterParams.num_cpus)(RegInit(0.U(64.W)))
    
    // Regulation
    val periodCntrReset = periodCntr >= periodLen
    val periodCntr = Reg(UInt(wPeriod.W))
   

    val LLCAccessCountersReg = AccessCounters.zipWithIndex.map{ case (reg, i) => 
        (0x20 + i * 8) -> Seq(RegField.r(reg.getWidth, reg, RegFieldDesc(s"LLCAccessCounterReg${i}", s"Total LLC accesses for domainId=${i}")))
    }
    val MissCounterOffset = (0x100)
    val LLCMissCountersReg = MissCounters.zipWithIndex.map{ case (reg, i) =>
        (MissCounterOffset + i * 8) -> Seq(RegField.r(reg.getWidth, reg, RegFieldDesc(s"LLCMissCounterReg${i}", s"Total LLC misses for domainId=${i}")))
    }
    val CountInstFetchReg = Seq((0x200) -> Seq(RegField(countInstFetch.getWidth, countInstFetch, RegFieldDesc("countInstFetch", "Bool count instruction fetches in access counters"))))
    val EnableIntRegs = EnableInterrupt.zipWithIndex.map { case (reg, i) =>
        (0x208 + i*0x8)-> Seq(RegField(reg.getWidth, reg, RegFieldDesc(s"EnableInterrupt Core ${i}", s"Enable Interrupts Core ${i}")))
    } 
    val CoreBudgetRegs = CoreBudgets.zipWithIndex.map { case (reg, i) => 
        (0x300 + i * 8) -> Seq(RegField.r(reg.getWidth, reg, RegFieldDesc(s"CoreBudget Core${i}", s"CoreBudget Core${i}")))
    }
    val PeriodSetReg = Seq((0x400) -> Seq(RegField.r(periodLen.getWidth, periodLen, RegFieldDesc(s"PeriodLength", s"Period Length"))))



    periodCntr := Mux(periodCntrReset, 0.U, periodCntr + 1.U)

    when (periodCntrReset) // Reset all
    {
        for (i <- 0 until nBanks)
        {
          for (j <- 0 until accCounterParams.num_cpus)
          {
            PerBankAccessCounters(i)(j) := 0.U
            PerBankMissCounters(i)(j) := 0.U
          }
        }

        for (j <- 0 until accCounterParams.num_cpus)
        {
            MissCounters(j) := 0.U
            AccessCounters(j) := 0.U
        }

    }
    .otherwise // Calculate per core total accesses
    {
        for (j <- 0 until accCounterParams.num_cpus)
        {
          val tmpSumMiss = VecInit(Seq.fill(nBanks)(0.U(64.W)))
          val tmpSumAccess = VecInit(Seq.fill(nBanks)(0.U(64.W)))
          tmpSumMiss(0) := PerBankMissCounters(0)(j)
          tmpSumAccess(0) := PerBankAccessCounters(0)(j)
          for (i <- 1 until nBanks)
          {
            tmpSumMiss(i) := PerBankMissCounters(i)(j) + tmpSumMiss(i-1)
            tmpSumAccess(i) := PerBankAccessCounters(i)(j) + tmpSumAccess(i-1)
          }

          MissCounters(j) := tmpSumMiss(nBanks - 1)
          AccessCounters(j) := tmpSumAccess(nBanks- 1)
        }    
    }

    (io.TLIn zip io.TLOut).zipWithIndex.map { case (((in), (out)), i) => {
        val inDomainID = Mux(in.a.fire, in.a.bits.domainId, Mux(in.c.fire, in.c.bits.domainId, 0.U))
        val outDomainID = Mux(out.a.fire, out.a.bits.domainId, Mux(out.c.fire, out.c.bits.domainId, 0.U))
        val aIsAcquire = in.a.bits.opcode === TLMessages.AcquireBlock
        val aIsInstFetch = in.a.bits.opcode === TLMessages.Get && in.a.bits.address >= memBase
        val aIsRead = aIsAcquire || (aIsInstFetch && countInstFetch)
        val aIsWrite = (in.a.bits.opcode === TLMessages.PutFullData || in.a.bits.opcode === TLMessages.PutPartialData) && in.a.bits.address >= memBase
        val cIsWb = in.c.bits.opcode === TLMessages.ReleaseData || in.c.bits.opcode === TLMessages.ProbeAckData
        val outaIsAcquire = out.a.bits.opcode === TLMessages.AcquireBlock
        val outaIsInstFetch = out.a.bits.opcode === TLMessages.Get && out.a.bits.address >= memBase
        val outCIsWb =  in.c.bits.opcode === TLMessages.ReleaseData || in.c.bits.opcode === TLMessages.ProbeAckData
        val isMiss = (outaIsAcquire || (outaIsInstFetch && countInstFetch)) && out.a.fire
        val isWbToDRAM = (outCIsWb && out.c.fire)
        val toDRAM = (isMiss || isWbToDRAM)
        val isAccess = ((aIsWrite || aIsRead || (aIsInstFetch && countInstFetch)) && in.a.fire) || (cIsWb && in.c.fire) 
        when (!periodCntrReset)
        {
            when (toDRAM)
            {
                PerBankMissCounters(i)(outDomainID)  := PerBankMissCounters(i)(outDomainID) + 1.U
            }
            when (isAccess)
            {
                PerBankAccessCounters(i)(inDomainID) := PerBankAccessCounters(i)(inDomainID) + 1.U
            }
        }
    }}

    /*
        Generate interrupt when counter exceeds budget
    */
    for (i <- 0 until accCounterParams.num_cpus)
    {
        io.interrupt(i) := (MissCounters(i) >= CoreBudgets(i) && EnableInterrupt(i))
    }

    def YieldRegisters() : Seq[(Int, Seq[RegField])] = {
        val mmreg = LLCAccessCountersReg ++ LLCMissCountersReg ++ CountInstFetchReg ++ CoreBudgetRegs ++ PeriodSetReg
        mmreg
    }
}

class InclusiveCache(
  val cache: CacheParameters,
  val micro: InclusiveCacheMicroParameters,
  control: Option[InclusiveCacheControlParameters] = None
  )(implicit p: Parameters)
    extends LazyModule
{
  val access = TransferSizes(1, cache.blockBytes)
  val xfer = TransferSizes(cache.blockBytes, cache.blockBytes)
  val atom = TransferSizes(1, cache.beatBytes)

  var resourcesOpt: Option[ResourceBindings] = None

  val device: SimpleDevice = new SimpleDevice("cache-controller", Seq("sifive,inclusivecache0", "cache")) {
    def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))
    
    override def describe(resources: ResourceBindings): Description = {
      
      val Description(name, mapping) = super.describe(resources)
      resourcesOpt = Some(resources)
      //val int = describeInterrupts(resources)      // interrupt description
      // Find the outer caches
      val outer = node.edges.out
        .flatMap(_.manager.managers)
        .filter(_.supportsAcquireB)
        .flatMap(_.resources.headOption)
        .map(_.owner.label)
        .distinct
      val nextlevel: Option[(String, Seq[ResourceValue])] =
        if (outer.isEmpty) {
          None
        } else {
          Some("next-level-cache" -> outer.map(l => ResourceReference(l)).toList)
        }

      val extra = Map(
        "cache-level"            -> ofInt(2),
        "cache-unified"          -> Nil,
        "cache-size"             -> ofInt(cache.sizeBytes * node.edges.in.size),
        "cache-sets"             -> ofInt(cache.sets * node.edges.in.size),
        "cache-block-size"       -> ofInt(cache.blockBytes),
        "sifive,mshr-count"      -> ofInt(InclusiveCacheParameters.all_mshrs(cache, micro)),
        )
        
      Description(name, mapping ++ extra ++ nextlevel)
    }
  }

  val node: TLAdapterNode = TLAdapterNode(
    clientFn  = { _ => TLClientPortParameters(Seq(TLClientParameters(
      name          = s"L${cache.level} InclusiveCache",
      sourceId      = IdRange(0, InclusiveCacheParameters.out_mshrs(cache, micro)),
      supportsProbe = xfer)))
    },
    managerFn = { m => TLManagerPortParameters(
      managers = m.managers.map { m => m.copy(
        regionType         = if (m.regionType >= RegionType.UNCACHED) RegionType.CACHED else m.regionType,
        resources          = Resource(device, "caches") +: m.resources,
        supportsAcquireB   = xfer,
        supportsAcquireT   = if (m.supportsAcquireT) xfer else TransferSizes.none,
        supportsArithmetic = if (m.supportsAcquireT) atom else TransferSizes.none,
        supportsLogical    = if (m.supportsAcquireT) atom else TransferSizes.none,
        supportsGet        = access,
        supportsPutFull    = if (m.supportsAcquireT) access else TransferSizes.none,
        supportsPutPartial = if (m.supportsAcquireT) access else TransferSizes.none,
        supportsHint       = access,
        alwaysGrantsT      = false,
        fifoId             = None)
      },
      beatBytes  = cache.beatBytes,
      endSinkId  = InclusiveCacheParameters.all_mshrs(cache, micro),
      minLatency = 2)
    })
  
  val ctlnode = control.map { c => TLRegisterNode(
    address     = Seq(AddressSet(c.address, 0xfff)),
    device      = device,
    concurrency = 1, // Only one flush at a time (else need to track who answers)
    beatBytes   = c.beatBytes)}
  
   val intSrc = IntSourceNode(IntSourcePortSimple(num = 1, resources = device.int))
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {

    val bundleParams = node.out(0)._1.params
    val CounterModule = new CacheAccessCounter(CacheAccessCounterParams(cache.numCPUs, bundleParams))
    
    // If you have a control port, you must have at least one cache port
    require (!ctlnode.isDefined || !node.edges.in.isEmpty)

    // Extract the client IdRanges; must be the same on all ports!
    val clientIds = node.edges.in.headOption.map(_.client.clients.map(_.sourceId).sortBy(_.start))
    node.edges.in.foreach { e => require(e.client.clients.map(_.sourceId).sortBy(_.start) == clientIds.get) }

    // Use the natural ordering of clients (just like in Directory)
    node.edges.in.headOption.foreach { n =>
      println(s"L${cache.level} InclusiveCache Client Map:")
      n.client.clients.zipWithIndex.foreach { case (c,i) =>
        println(s"\t${i} <= ${c.name}")
      }
      println("")
    }

    // Flush directive
    val flushInValid   = RegInit(false.B)
    val flushInReady   = WireInit(init = false.B)
    val flushInAddress = Reg(UInt(64.W))
    val flushNoMatch   = WireInit(init = true.B)
    val flushOutValid  = RegInit(false.B)
    val flushOutReady  = WireInit(init = false.B)

    when (flushOutReady) { flushOutValid := false.B }
    when (flushInReady)  { flushInValid  := false.B }

    when (flushNoMatch && flushInValid) {
      flushInReady := true.B
      flushOutValid := true.B
    }


    /*
      Performance Counters that we added
    */
    assert(node.out.length == node.in.length)

   
    for (i <- 0 until cache.numCPUs)
    {
      val (intOut, _) = intSrc.out(0) // does this need to be i as well? 
      intOut(i) := CounterModule.module.io.interrupt(i)
    }
    

    val flush32 = RegField.w(32, RegWriteFn((ivalid, oready, data) => {
      when (oready) { flushOutReady := true.B }
      when (ivalid) { flushInValid := true.B }
      when (ivalid && !flushInValid) { flushInAddress := data << 4 }
      (!flushInValid, flushOutValid)
    }), RegFieldDesc("Flush32", "Flush the physical address equal to the 32-bit written data << 4 from the cache"))

    val flush64 = RegField.w(64, RegWriteFn((ivalid, oready, data) => {
      when (oready) { flushOutReady := true.B }
      when (ivalid) { flushInValid := true.B }
      when (ivalid && !flushInValid) { flushInAddress := data }
      (!flushInValid, flushOutValid)
    }), RegFieldDesc("Flush64", "Flush the phsyical address equal to the 64-bit written data from the cache"))

    // Information about the cache configuration
    // starts at 0x2010000
    val banksR  = Seq(0 -> Seq(RegField.r(8, node.edges.in.size.U,               RegFieldDesc("Banks",
      "Number of banks in the cache", reset=Some(node.edges.in.size)))))
    val waysR   = Seq(0x8 -> Seq(RegField.r(8, cache.ways.U,                       RegFieldDesc("Ways",
      "Number of ways per bank", reset=Some(cache.ways)))))
    val lgSetsR = Seq(0x10 -> Seq(RegField.r(8, log2Ceil(cache.sets).U,             RegFieldDesc("lgSets",
      "Base-2 logarithm of the sets per bank", reset=Some(log2Ceil(cache.sets))))))
    val lgBlockBytesR = Seq(0x18 -> Seq(RegField.r(8, log2Ceil(cache.blockBytes).U, RegFieldDesc("lgBlockBytes",
      "Base-2 logarithm of the bytes per cache block", reset=Some(log2Ceil(cache.blockBytes))))))
    val flush64Reg = Seq(0x200 ->  Seq(flush64))
    val flush32Reg = Seq(0x240 -> Seq(flush32))

    val mmreg = banksR ++ waysR ++ lgSetsR ++ lgBlockBytesR ++ CounterModule.module.YieldRegisters() ++ flush64Reg ++ flush32Reg
    


    val regmap = ctlnode.map{ c =>
      c.regmap(mmreg: _*)
    }

   // val regmap = ctlnode.map { c =>
   //   c.regmap(
   //     0x000 -> RegFieldGroup("Config", Some("Information about the Cache Configuration"), Seq(banksR, waysR, lgSetsR, lgBlockBytesR, LLCAccessCounterReg, LLCMissCounterReg,
   //     LLCAccessCounterReset, CountInstFetchReg)),
   //     0x200 -> (if (control.get.beatBytes >= 8) Seq(flush64) else Seq()),
   //     0x240 -> Seq(flush32)
   //   )
   // }

    

    // Create the L2 Banks
    val mods = (node.in zip node.out).zipWithIndex map { case (((in, edgeIn), (out, edgeOut)), i) =>
      
      edgeOut.manager.managers.foreach { m =>
        require (m.supportsAcquireB.contains(xfer),
          s"All managers behind the L2 must support acquireB($xfer) " +
          s"but ${m.name} only supports (${m.supportsAcquireB})!")
        if (m.supportsAcquireT) require (m.supportsAcquireT.contains(xfer),
          s"Any probing managers behind the L2 must support acquireT($xfer) " +
          s"but ${m.name} only supports (${m.supportsAcquireT})!")
      }

      val params = InclusiveCacheParameters(cache, micro, control.isDefined, edgeIn, edgeOut)
      val scheduler = Module(new InclusiveCacheBankScheduler(params)).suggestName("inclusive_cache_bank_sched")
      
      /* 
        We cannot do this here because the system bus sits between the L2 and the L1

        These in edges are not directly connected to the core Tiles
      */
      //in.a.bits.domainId := i.U // set the domainID based on the input edge index
      scheduler.io.in <> in
      

      out <> scheduler.io.out
      

      /* Performance Counters */



      scheduler.io.ways := DontCare
      scheduler.io.divs := DontCare

      val flushSelect = edgeIn.manager.managers.flatMap(_.address).map(_.contains(flushInAddress)).reduce(_||_)
      when (flushSelect) { flushNoMatch := false.B }

      when (flushSelect && scheduler.io.req.ready)  { flushInReady := true.B }
      when (scheduler.io.resp.valid) { flushOutValid := true.B }
      assert (!scheduler.io.resp.valid || flushSelect)

      scheduler.io.req.valid := flushInValid && flushSelect
      scheduler.io.req.bits.address := flushInAddress
      scheduler.io.resp.ready := !flushOutValid

      // Fix-up the missing addresses. We do this here so that the Scheduler can be
      // deduplicated by Firrtl to make hierarchical place-and-route easier.

      out.a.bits.address := params.restoreAddress(scheduler.io.out.a.bits.address)
      in .b.bits.address := params.restoreAddress(scheduler.io.in .b.bits.address)
      out.c.bits.address := params.restoreAddress(scheduler.io.out.c.bits.address)

      scheduler
    }

    def json = s"""{"banks":[${mods.map(_.json).mkString(",")}]}"""
  }
}
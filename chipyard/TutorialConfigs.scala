// chipyard/generators/chipyard/src/main/scala/config
package chipyard

import freechips.rocketchip.config.{Config}

import chisel3._
import MAC._
import CONV._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

// This file is designed to accompany a live tutorial, with slides.
// For each of 4 phases, participants will customize and build a
// small demonstration config.

// This file is designed to be used after running chipyard/scripts/tutorial-setup.sh,
// which removes the SHA3 accelerator RTL, and provides participants
// the experience of integrating external RTL.

// This file was originally developed for the cancelled ASPLOS-2020
// Chipyard tutorial. While the configs here work, the corresponding
// slideware has not yet been created.

// NOTE: Configs should be read bottom-up, since they are applied bottom-up

// NOTE: The TutorialConfigs build off of the AbstractConfig defined in AbstractConfig.scala
//       Users should try to understand the functionality of the AbstractConfig before proceeding
//       with the TutorialConfigs below

// Tutorial Phase 1: Configure the cores, caches
class TutorialStarterConfig extends Config(
  // CUSTOMIZE THE CORE
  // Uncomment out one (or multiple) of the lines below, and choose
  // how many cores you want.
  // new freechips.rocketchip.subsystem.WithNBigCores(1) ++    // Specify we want some number of Rocket cores
  // new boom.common.WithNSmallBooms(1) ++                     // Specify we want some number of BOOM cores

  // CUSTOMIZE the L2
  // Uncomment this line, and specify a size if you want to have a L2
  // new freechips.rocketchip.subsystem.WithInclusiveCache(nBanks=1, nWays=4, capacityKB=128) ++

  new chipyard.config.AbstractConfig
)

// Tutorial Phase 2: Integrate a TileLink or AXI4 MMIO device
class TutorialMMIOConfig extends Config(

  // Attach either a TileLink or AXI4 version of GCD
  // Uncomment one of the below lines
  // new chipyard.example.WithGCD(useAXI4=false) ++ // Use TileLink version
  // new chipyard.example.WithGCD(useAXI4=true) ++  // Use AXI4 version

  // For this demonstration we assume the base system is a single-core Rocket, for fast elaboration
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new chipyard.config.AbstractConfig
)

// Tutorial Phase 3: Integrate a SHA3 RoCC accelerator
class TutorialSha3Config extends Config(
  // Uncomment this line once you added SHA3 to the build.sbt, and cloned the SHA3 repo
  // new sha3.WithSha3Accel ++

  // For this demonstration we assume the base system is a single-core Rocket, for fast elaboration
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new chipyard.config.AbstractConfig
)

// Tutorial Phase 4: Integrate a Black-box verilog version of the SHA3 RoCC accelerator
class TutorialSha3BlackBoxConfig extends Config(
  // Uncomment these lines once SHA3 is integrated
  // new sha3.WithSha3BlackBox ++ // Specify we want the Black-box verilog version of Sha3 Ctrl
  // new sha3.WithSha3Accel ++

  // For this demonstration we assume the base system is a single-core Rocket, for fast elaboration
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new chipyard.config.AbstractConfig
)

class MyConfig extends Config(
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new chipyard.config.AbstractConfig
)

class MyBOOMConfig extends Config(
  new boom.common.WithNSmallBooms(1) ++ 
  new chipyard.config.AbstractConfig
)

class MySha3Config extends Config(
  new sha3.WithSha3Accel ++
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new chipyard.config.AbstractConfig
)

class WithMyMAC extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p:Parameters) => {
      val MAC = LazyModule.apply(new MyMAC(OpcodeSet.custom0)(p))
      MAC
    }
  )
})

class MyMACConfig extends Config(
  new WithMyMAC ++
  new MyConfig
)

class WithOurCONV extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      val conv = LazyModule(new CONV.OurCONV(OpcodeSet.custom0)(p))
      conv
    }
  )
})

class OurCONVConfig extends Config(
	new WithOurCONV ++ 
	new MyConfig
)
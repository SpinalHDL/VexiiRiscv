package vexiiriscv.fetch

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline
import vexiiriscv.misc.CtrlPipelinePlugin

import scala.collection.mutable

class FetchPipelinePlugin extends CtrlPipelinePlugin
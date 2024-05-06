//package spinal.lib.misc
//
//import spinal.core._
//import spinal.core.internals.{BaseNode, Expression}
//
//import scala.collection.mutable
//import scala.collection.mutable.ArrayBuffer
//
//class AstWalker {
//  def apply(from: BaseNode)(body : (BaseNode, Int) => Boolean): Unit = {
//    val walkedId = GlobalData.get.allocateAlgoIncrementale()
//
//    rec(from)
//    def rec(driver : BaseNode) : Unit = {
//      foreach(from) { (node, latency) =>
//        if (body(node, latency)) {
//          rec(driver)
//        }
//      }
//    }
//
//    def foreach(that: BaseNode)(onUp : (BaseNode, Int) => Unit): Unit = {
//      if(that.algoIncrementale == walkedId)
//        return
//      that.algoIncrementale = walkedId
//      if(that == from)
//        return
//
//      that match{
//        case that : Mem[_] => {
//          that.foreachStatements{
//            case port : MemWrite =>
//              port.foreachDrivingExpression(input => {
//                onUp(input, 1)
//              })
//            case port : MemReadWrite =>
//              port.foreachDrivingExpression(input => {
//                onUp(input, 1)
//              })
//            case port : MemReadSync =>
//            case port : MemReadAsync =>
//            //TODO other ports
//          }
//        }
//        case that : BaseType => { //TODO IR when conds
//          def foreachInputs(func : (BaseNode) => Unit) = {
//            that.foreachStatements(s => {
//              s.foreachDrivingExpression(input => {
//                func(input)
//              })
//              s.walkParentTreeStatementsUntilRootScope(tree => tree.foreachDrivingExpression(input => {
//                func(input)
//              }))
//            })
//          }
//          if(that.isReg){
//            foreachInputs(input => onUp(input, 1))
//          } else {
//            foreachInputs(input => {
//              onUp(input, 0)
//            })
//          }
//        }
//        case that : MemReadSync =>
//          that.foreachDrivingExpression(input => onUp(input, 1))
//          onUp(that.mem, 1)
//        case that : MemReadWrite =>
//          that.foreachDrivingExpression{input =>
//            val lat = if(input == that.data || input == that.mask) 2 else 1
//            onUp(input, 1)
//          }
//          onUp(that.mem, 1)
//        case that : MemReadAsync =>
//          that.foreachDrivingExpression(input => {
//            onUp(input, 0)
//          })
//          onUp(that.mem,0)
//        case that : Expression => {
//          that.foreachDrivingExpression(input => {
//            onUp(input, 0)
//          })
//        }
//      }
//    }
//
//  }
//}

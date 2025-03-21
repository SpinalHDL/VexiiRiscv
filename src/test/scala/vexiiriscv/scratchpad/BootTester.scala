package vexiiriscv.scratchpad

import jssc.{SerialPort, SerialPortException}
import spinal.lib.DoCmd

/**
 * This is hardware test utility which cycle debian boot again and again to test stability.
 * - Assume a FPGA board is preconfigured with a litex bitstream and boot debian automaticaly on power-on.
 * - This utilty connect to a ttyUSB to monitor and sent stimulus to the board's debian.
 * - Once a full boot/shutdown sequance id done, it restart the board using openocd/JTAG
 */
object BootTester extends App{
  var tty = "/dev/ttyUSB2"
  var openocdReboot = "openocd -f ft2232h_breakout.cfg  -f vexiiriscv_jtag.tcl -f debug.tcl"

  assert(new scopt.OptionParser[Unit]("tool") {
    opt[String]("tty") action { (v, c) => tty = v }
    opt[String]("openocd-reboot") action { (v, c) => openocdReboot = v }
  }.parse(args, ()).nonEmpty)

  var counter = 0
  val serialPort = new SerialPort(tty)
  var state = "LOGIN"
  def tx(that : String): Unit = {
    for(c <- that){
      serialPort.writeByte(c.toByte)
      Thread.sleep(50)
    }
  }

  def reboot(): Unit = {
    DoCmd.doCmd(openocdReboot, "src/main/tcl/openocd")
  }

  def notify(line : String): Unit = {
    state match {
      case "LOGIN" => if(line.endsWith("nexys login:")){
        println("BOOTED")
        Thread.sleep(2000)
        tx("root\n")
        state = "PASSWORD"
      }
      case "PASSWORD" => if(line.endsWith("Password:")){
        println("BOOTED")
        Thread.sleep(2000)
        tx("root\n")
        state = "LOGGED"
      }

      case "LOGGED" => if(line.endsWith("root@nexys:")){
        println("LOGGED")
        Thread.sleep(2000)
        tx("poweroff\n")
        state = "LITEX"
      }
      case "LITEX" => if(line.endsWith("reboot: Power down")){
        println("LITEX")
        Thread.sleep(2000)
        reboot()
        counter += 1
        println("Reboot counter " + counter)
        state = "LOGIN"
      }
    }
  }

  reboot()
  try {
    var line = new StringBuilder()
    serialPort.openPort //Open serial port
    serialPort.setParams(115200, 8, 1, 0) //Set params.
    while(true){
      val buffer = serialPort.readString()
      if(buffer!=null){
        print(buffer)
        buffer.foreach{
          case '\n' => line.clear()
          case '\r' =>
          case c => line += c; notify(line.toString);
        }
      } else {
        Thread.sleep(10)
      }
    }

    serialPort.closePort //Close serial port
  } catch {
    case ex: SerialPortException =>
      System.out.println(ex)
  }
}

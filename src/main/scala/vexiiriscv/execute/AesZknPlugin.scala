package vexiiriscv.execute

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.decode.{Decode, DecoderService}
import vexiiriscv.riscv.{IntRegFile, RS1, RS2, Riscv, Rvk}

import scala.collection.mutable.ArrayBuffer

/**
 * This implement the official RISC-V AES instruction.
 * See https://github.com/SpinalHDL/VexRiscv/blob/dev/src/main/scala/vexriscv/plugin/AesZknPlugin.scala / RISC-V doc
 */
object AesZknPlugin {
  def make(layer: LaneLayer,
           xlen: Int,
           readAt: Int = 0,
           writeBackAt: Int = 2) = {

    val plugins = ArrayBuffer[FiberPlugin]()

    xlen match {
      case 32 => {
        plugins += new Aes32ZknPlugin(
          layer       = layer,
          readAt      = readAt,
          writeBackAt = writeBackAt,
        )
      }
      case 64 => {
        plugins += new Aes64MainZknPlugin(
          layer       = layer,
          readAt      = readAt,
          writeBackAt = writeBackAt,
        )

        /* As this is simple, advance the write back stage */
        plugins += new Aes64Ks2ZknPlugin(
          layer       = layer,
          readAt      = readAt,
          writeBackAt = writeBackAt - 1,
        )
      }
    }

    plugins
  }

  // Encryption table which solve a single byte sbox + column mix. Used for all rounds
  def TE0 = List(
    0xa5c663, 0x84f87c, 0x99ee77, 0x8df67b,
    0x0dfff2, 0xbdd66b, 0xb1de6f, 0x5491c5,
    0x506030, 0x030201, 0xa9ce67, 0x7d562b,
    0x19e7fe, 0x62b5d7, 0xe64dab, 0x9aec76,
    0x458fca, 0x9d1f82, 0x4089c9, 0x87fa7d,
    0x15effa, 0xebb259, 0xc98e47, 0x0bfbf0,
    0xec41ad, 0x67b3d4, 0xfd5fa2, 0xea45af,
    0xbf239c, 0xf753a4, 0x96e472, 0x5b9bc0,
    0xc275b7, 0x1ce1fd, 0xae3d93, 0x6a4c26,
    0x5a6c36, 0x417e3f, 0x02f5f7, 0x4f83cc,
    0x5c6834, 0xf451a5, 0x34d1e5, 0x08f9f1,
    0x93e271, 0x73abd8, 0x536231, 0x3f2a15,
    0x0c0804, 0x5295c7, 0x654623, 0x5e9dc3,
    0x283018, 0xa13796, 0x0f0a05, 0xb52f9a,
    0x090e07, 0x362412, 0x9b1b80, 0x3ddfe2,
    0x26cdeb, 0x694e27, 0xcd7fb2, 0x9fea75,
    0x1b1209, 0x9e1d83, 0x74582c, 0x2e341a,
    0x2d361b, 0xb2dc6e, 0xeeb45a, 0xfb5ba0,
    0xf6a452, 0x4d763b, 0x61b7d6, 0xce7db3,
    0x7b5229, 0x3edde3, 0x715e2f, 0x971384,
    0xf5a653, 0x68b9d1, 0x000000, 0x2cc1ed,
    0x604020, 0x1fe3fc, 0xc879b1, 0xedb65b,
    0xbed46a, 0x468dcb, 0xd967be, 0x4b7239,
    0xde944a, 0xd4984c, 0xe8b058, 0x4a85cf,
    0x6bbbd0, 0x2ac5ef, 0xe54faa, 0x16edfb,
    0xc58643, 0xd79a4d, 0x556633, 0x941185,
    0xcf8a45, 0x10e9f9, 0x060402, 0x81fe7f,
    0xf0a050, 0x44783c, 0xba259f, 0xe34ba8,
    0xf3a251, 0xfe5da3, 0xc08040, 0x8a058f,
    0xad3f92, 0xbc219d, 0x487038, 0x04f1f5,
    0xdf63bc, 0xc177b6, 0x75afda, 0x634221,
    0x302010, 0x1ae5ff, 0x0efdf3, 0x6dbfd2,
    0x4c81cd, 0x14180c, 0x352613, 0x2fc3ec,
    0xe1be5f, 0xa23597, 0xcc8844, 0x392e17,
    0x5793c4, 0xf255a7, 0x82fc7e, 0x477a3d,
    0xacc864, 0xe7ba5d, 0x2b3219, 0x95e673,
    0xa0c060, 0x981981, 0xd19e4f, 0x7fa3dc,
    0x664422, 0x7e542a, 0xab3b90, 0x830b88,
    0xca8c46, 0x29c7ee, 0xd36bb8, 0x3c2814,
    0x79a7de, 0xe2bc5e, 0x1d160b, 0x76addb,
    0x3bdbe0, 0x566432, 0x4e743a, 0x1e140a,
    0xdb9249, 0x0a0c06, 0x6c4824, 0xe4b85c,
    0x5d9fc2, 0x6ebdd3, 0xef43ac, 0xa6c462,
    0xa83991, 0xa43195, 0x37d3e4, 0x8bf279,
    0x32d5e7, 0x438bc8, 0x596e37, 0xb7da6d,
    0x8c018d, 0x64b1d5, 0xd29c4e, 0xe049a9,
    0xb4d86c, 0xfaac56, 0x07f3f4, 0x25cfea,
    0xafca65, 0x8ef47a, 0xe947ae, 0x181008,
    0xd56fba, 0x88f078, 0x6f4a25, 0x725c2e,
    0x24381c, 0xf157a6, 0xc773b4, 0x5197c6,
    0x23cbe8, 0x7ca1dd, 0x9ce874, 0x213e1f,
    0xdd964b, 0xdc61bd, 0x860d8b, 0x850f8a,
    0x90e070, 0x427c3e, 0xc471b5, 0xaacc66,
    0xd89048, 0x050603, 0x01f7f6, 0x121c0e,
    0xa3c261, 0x5f6a35, 0xf9ae57, 0xd069b9,
    0x911786, 0x5899c1, 0x273a1d, 0xb9279e,
    0x38d9e1, 0x13ebf8, 0xb32b98, 0x332211,
    0xbbd269, 0x70a9d9, 0x89078e, 0xa73394,
    0xb62d9b, 0x223c1e, 0x921587, 0x20c9e9,
    0x4987ce, 0xffaa55, 0x785028, 0x7aa5df,
    0x8f038c, 0xf859a1, 0x800989, 0x171a0d,
    0xda65bf, 0x31d7e6, 0xc68442, 0xb8d068,
    0xc38241, 0xb02999, 0x775a2d, 0x111e0f,
    0xcb7bb0, 0xfca854, 0xd66dbb, 0x3a2c16
  )


  // Decryption table which solve a single byte sbox + column mix. Not used in the last round
  def TD0 = List(
    0x50a7f451l, 0x5365417el, 0xc3a4171al, 0x965e273al,
    0xcb6bab3bl, 0xf1459d1fl, 0xab58faacl, 0x9303e34bl,
    0x55fa3020l, 0xf66d76adl, 0x9176cc88l, 0x254c02f5l,
    0xfcd7e54fl, 0xd7cb2ac5l, 0x80443526l, 0x8fa362b5l,
    0x495ab1del, 0x671bba25l, 0x980eea45l, 0xe1c0fe5dl,
    0x02752fc3l, 0x12f04c81l, 0xa397468dl, 0xc6f9d36bl,
    0xe75f8f03l, 0x959c9215l, 0xeb7a6dbfl, 0xda595295l,
    0x2d83bed4l, 0xd3217458l, 0x2969e049l, 0x44c8c98el,
    0x6a89c275l, 0x78798ef4l, 0x6b3e5899l, 0xdd71b927l,
    0xb64fe1bel, 0x17ad88f0l, 0x66ac20c9l, 0xb43ace7dl,
    0x184adf63l, 0x82311ae5l, 0x60335197l, 0x457f5362l,
    0xe07764b1l, 0x84ae6bbbl, 0x1ca081fel, 0x942b08f9l,
    0x58684870l, 0x19fd458fl, 0x876cde94l, 0xb7f87b52l,
    0x23d373abl, 0xe2024b72l, 0x578f1fe3l, 0x2aab5566l,
    0x0728ebb2l, 0x03c2b52fl, 0x9a7bc586l, 0xa50837d3l,
    0xf2872830l, 0xb2a5bf23l, 0xba6a0302l, 0x5c8216edl,
    0x2b1ccf8al, 0x92b479a7l, 0xf0f207f3l, 0xa1e2694el,
    0xcdf4da65l, 0xd5be0506l, 0x1f6234d1l, 0x8afea6c4l,
    0x9d532e34l, 0xa055f3a2l, 0x32e18a05l, 0x75ebf6a4l,
    0x39ec830bl, 0xaaef6040l, 0x069f715el, 0x51106ebdl,
    0xf98a213el, 0x3d06dd96l, 0xae053eddl, 0x46bde64dl,
    0xb58d5491l, 0x055dc471l, 0x6fd40604l, 0xff155060l,
    0x24fb9819l, 0x97e9bdd6l, 0xcc434089l, 0x779ed967l,
    0xbd42e8b0l, 0x888b8907l, 0x385b19e7l, 0xdbeec879l,
    0x470a7ca1l, 0xe90f427cl, 0xc91e84f8l, 0x00000000l,
    0x83868009l, 0x48ed2b32l, 0xac70111el, 0x4e725a6cl,
    0xfbff0efdl, 0x5638850fl, 0x1ed5ae3dl, 0x27392d36l,
    0x64d90f0al, 0x21a65c68l, 0xd1545b9bl, 0x3a2e3624l,
    0xb1670a0cl, 0x0fe75793l, 0xd296eeb4l, 0x9e919b1bl,
    0x4fc5c080l, 0xa220dc61l, 0x694b775al, 0x161a121cl,
    0x0aba93e2l, 0xe52aa0c0l, 0x43e0223cl, 0x1d171b12l,
    0x0b0d090el, 0xadc78bf2l, 0xb9a8b62dl, 0xc8a91e14l,
    0x8519f157l, 0x4c0775afl, 0xbbdd99eel, 0xfd607fa3l,
    0x9f2601f7l, 0xbcf5725cl, 0xc53b6644l, 0x347efb5bl,
    0x7629438bl, 0xdcc623cbl, 0x68fcedb6l, 0x63f1e4b8l,
    0xcadc31d7l, 0x10856342l, 0x40229713l, 0x2011c684l,
    0x7d244a85l, 0xf83dbbd2l, 0x1132f9ael, 0x6da129c7l,
    0x4b2f9e1dl, 0xf330b2dcl, 0xec52860dl, 0xd0e3c177l,
    0x6c16b32bl, 0x99b970a9l, 0xfa489411l, 0x2264e947l,
    0xc48cfca8l, 0x1a3ff0a0l, 0xd82c7d56l, 0xef903322l,
    0xc74e4987l, 0xc1d138d9l, 0xfea2ca8cl, 0x360bd498l,
    0xcf81f5a6l, 0x28de7aa5l, 0x268eb7dal, 0xa4bfad3fl,
    0xe49d3a2cl, 0x0d927850l, 0x9bcc5f6al, 0x62467e54l,
    0xc2138df6l, 0xe8b8d890l, 0x5ef7392el, 0xf5afc382l,
    0xbe805d9fl, 0x7c93d069l, 0xa92dd56fl, 0xb31225cfl,
    0x3b99acc8l, 0xa77d1810l, 0x6e639ce8l, 0x7bbb3bdbl,
    0x097826cdl, 0xf418596el, 0x01b79aecl, 0xa89a4f83l,
    0x656e95e6l, 0x7ee6ffaal, 0x08cfbc21l, 0xe6e815efl,
    0xd99be7bal, 0xce366f4al, 0xd4099feal, 0xd67cb029l,
    0xafb2a431l, 0x31233f2al, 0x3094a5c6l, 0xc066a235l,
    0x37bc4e74l, 0xa6ca82fcl, 0xb0d090e0l, 0x15d8a733l,
    0x4a9804f1l, 0xf7daec41l, 0x0e50cd7fl, 0x2ff69117l,
    0x8dd64d76l, 0x4db0ef43l, 0x544daaccl, 0xdf0496e4l,
    0xe3b5d19el, 0x1b886a4cl, 0xb81f2cc1l, 0x7f516546l,
    0x04ea5e9dl, 0x5d358c01l, 0x737487fal, 0x2e410bfbl,
    0x5a1d67b3l, 0x52d2db92l, 0x335610e9l, 0x1347d66dl,
    0x8c61d79al, 0x7a0ca137l, 0x8e14f859l, 0x893c13ebl,
    0xee27a9cel, 0x35c961b7l, 0xede51ce1l, 0x3cb1477al,
    0x59dfd29cl, 0x3f73f255l, 0x79ce1418l, 0xbf37c773l,
    0xeacdf753l, 0x5baafd5fl, 0x146f3ddfl, 0x86db4478l,
    0x81f3afcal, 0x3ec468b9l, 0x2c342438l, 0x5f40a3c2l,
    0x72c31d16l, 0x0c25e2bcl, 0x8b493c28l, 0x41950dffl,
    0x7101a839l, 0xdeb30c08l, 0x9ce4b4d8l, 0x90c15664l,
    0x6184cb7bl, 0x70b632d5l, 0x745c6c48l, 0x4257b8d0l
  )

  // Last round decryption sbox
  def SBOX_INV = List(
    0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb,
    0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb,
    0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e,
    0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25,
    0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92,
    0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84,
    0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06,
    0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b,
    0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73,
    0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e,
    0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b,
    0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4,
    0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f,
    0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef,
    0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61,
    0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d
  )

  // Forward AES S-box, kept explicit to avoid rebuilding it from TE0.
  def SBOX = List(
    0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
    0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
    0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
    0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
    0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
    0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
    0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
    0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
    0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
    0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
    0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
    0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
    0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
    0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
    0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
  )
}

class Aes32ZknPlugin(
  val layer : LaneLayer,
  val readAt : Int = 0,
  val writeBackAt : Int = 2,
) extends ExecutionUnitElementSimple(layer){

  val mapping = new {
    def DECRYPT = 27
    def MIDDLE_ROUND = 26
    def BYTE_SEL = 30
  }

  val logic = during setup new Logic {
    awaitBuild()
    assert(Riscv.XLEN.get == 32)
    import SrcKeys._

    val wb = newWriteback(ifp, writeBackAt)

    val uopSpec = layer(add(Rvk.AES32_DE).uop)
    uopSpec.addRsSpec(RS1, readAt)
    uopSpec.addRsSpec(RS2, readAt)

    uopRetainer.release()

    // Hardware
    def BANK0 = (AesZknPlugin.TE0, AesZknPlugin.SBOX_INV).zipped.map((te0, inv) => (te0.toLong) | (inv.toLong << 24))
    def BANK1 =  AesZknPlugin.TD0

    val onRead = new el.Execute(readAt) {
      val byteSel = Decode.UOP(mapping.BYTE_SEL, 2 bits).asUInt
      val bankSel = Decode.UOP(mapping.DECRYPT) && Decode.UOP(mapping.MIDDLE_ROUND)
      val romAddress = U(bankSel ## up(el(IntRegFile, RS2)).subdivideIn(8 bits).read(byteSel))
    }

    val onData = new el.Execute(readAt + 1){
      //Decode the rom data
      val rom = new Area {
        val storage = Mem(Bits(32 bits), 512) initBigInt((BANK0 ++ BANK1).map(BigInt(_)))

        val data = storage.readSync(onRead.romAddress, isReady)
        val bytes = data.subdivideIn(8 bits)

        def VecUInt(l: Int*) = Vec(l.map(U(_, 2 bits)))
        // remap will be used to decode the rom
        val remap = Vec(
          VecUInt(2, 0, 0, 1),
          VecUInt(0, 0, 0, 0),
          VecUInt(3, 2, 1, 0),
          VecUInt(3, 3, 3, 3)
        )

        val address = U(Decode.UOP(mapping.DECRYPT) ## !Decode.UOP(mapping.MIDDLE_ROUND))
        val output = remap(address)
      }

      val wordDesuffle = new Area{
        val zero = B"0000"
        val byteSel = Decode.UOP(mapping.BYTE_SEL, 2 bits).asUInt
        val output = Vec(Bits(8 bits), 4)

        def remap(l : Int*) = Vec(l.map(rom.output(_)))
        val sel = byteSel.mux(
          0 -> remap(3, 2, 1, 0),
          1 -> remap(0, 3, 2, 1),
          2 -> remap(1, 0, 3, 2),
          3 -> remap(2, 1, 0, 3)
        )
        when(!Decode.UOP(mapping.MIDDLE_ROUND)){
          zero := B"1111"
          zero(byteSel) := False
        }

        // Finally, mux the rom data
        for(byteId <- 0 to 3){
          output(byteId) := rom.bytes(sel(byteId))
          when(zero(byteId)){
            output(byteId) := 0
          }
        }
      }

      val xored = wordDesuffle.output.asBits ^ up(el(IntRegFile, RS1))
      val CALC = insert(xored)
    }

    val onWb = new el.Execute(writeBackAt){
      wb.valid := SEL
      wb.payload := onData.CALC
    }
  }
}

class Aes64MainZknPlugin(
  val layer : LaneLayer,
  val readAt : Int = 0,
  val writeBackAt : Int = 2,
) extends ExecutionUnitElementSimple(layer) {
  val mapping = new {
    def DECRYPT = 27
    def MIDDLE_ROUND = 26

    def RCON = 23 downto 20
    def CHECK = 25 downto 24

    def CRYPTO = 25
    def KS = 24
  }

  def shiftRows(rs1: Vec[Bits], rs2: Vec[Bits]) = {
    Cat(rs1(3), rs2(6), rs2(1), rs1(4),
        rs2(7), rs2(2), rs1(5), rs1(0))
  }

  def invShiftRows(rs1: Vec[Bits], rs2: Vec[Bits]) = {
    Cat(rs2(3), rs2(6), rs1(1), rs1(4),
        rs1(7), rs2(2), rs2(5), rs1(0))
  }

  /*
   * XOR linear network for mixColumns
   * Result: 18 shared XORs + 90 output XORs, maximum logical depth 3.
   */
  def mixColumns32(value : Bits) = {
    val x = value.asBools
    val t0 = x(7) ^ x(15)
    val t1 = x(23) ^ x(31)
    val t2 = x(7) ^ x(31)
    val t3 = x(15) ^ x(23)
    val t4 = x(0) ^ x(8)
    val t5 = x(1) ^ x(9)
    val t6 = x(2) ^ x(10)
    val t7 = x(3) ^ x(11)
    val t8 = x(4) ^ x(12)
    val t9 = x(5) ^ x(13)
    val t10 = x(6) ^ x(14)
    val t11 = x(16) ^ x(24)
    val t12 = x(17) ^ x(25)
    val t13 = x(18) ^ x(26)
    val t14 = x(19) ^ x(27)
    val t15 = x(20) ^ x(28)
    val t16 = x(21) ^ x(29)
    val t17 = x(22) ^ x(30)
    val y0 = t11 ^ (x(8) ^ t0)
    val y1 = (x(9) ^ t0) ^ (t4 ^ t12)
    val y2 = t13 ^ (x(10) ^ t5)
    val y3 = (x(11) ^ t0) ^ (t6 ^ t14)
    val y4 = (x(12) ^ t0) ^ (t7 ^ t15)
    val y5 = t16 ^ (x(13) ^ t8)
    val y6 = t17 ^ (x(14) ^ t9)
    val y7 = t10 ^ (x(15) ^ t1)
    val y8 = t11 ^ (x(0) ^ t3)
    val y9 = (x(16) ^ t3) ^ (t12 ^ (x(1) ^ x(8)))
    val y10 = (x(2) ^ x(9)) ^ (x(17) ^ t13)
    val y11 = (x(18) ^ t3) ^ (t14 ^ (x(3) ^ x(10)))
    val y12 = (x(19) ^ t3) ^ (t15 ^ (x(4) ^ x(11)))
    val y13 = (x(5) ^ x(12)) ^ (x(20) ^ t16)
    val y14 = (x(6) ^ x(13)) ^ (x(21) ^ t17)
    val y15 = (x(7) ^ x(14)) ^ (x(22) ^ t1)
    val y16 = t4 ^ (x(24) ^ t1)
    val y17 = (x(25) ^ t1) ^ (t5 ^ t11)
    val y18 = t12 ^ (x(26) ^ t6)
    val y19 = (x(27) ^ t1) ^ (t7 ^ t13)
    val y20 = (x(28) ^ t1) ^ (t8 ^ t14)
    val y21 = t15 ^ (x(29) ^ t9)
    val y22 = t16 ^ (x(30) ^ t10)
    val y23 = t17 ^ (x(31) ^ t0)
    val y24 = t4 ^ (x(16) ^ t2)
    val y25 = (x(24) ^ t2) ^ (t5 ^ (x(0) ^ x(17)))
    val y26 = (x(1) ^ x(18)) ^ (x(25) ^ t6)
    val y27 = (x(26) ^ t2) ^ (t7 ^ (x(2) ^ x(19)))
    val y28 = (x(27) ^ t2) ^ (t8 ^ (x(3) ^ x(20)))
    val y29 = (x(4) ^ x(21)) ^ (x(28) ^ t9)
    val y30 = (x(5) ^ x(22)) ^ (x(29) ^ t10)
    val y31 = (x(6) ^ x(23)) ^ (x(30) ^ t0)

    Cat(y31, y30, y29, y28, y27, y26, y25, y24,
        y23, y22, y21, y20, y19, y18, y17, y16,
        y15, y14, y13, y12, y11, y10, y9,  y8,
        y7,  y6,  y5,  y4,  y3,  y2,  y1,  y0)
  }

  /*
   * XOR linear network for invMixColumns
   * Result: 61 shared XORs + 109 output XORs, maximum logical depth 6.
   */
  def invMixColumns32(value : Bits) = {
    val x = value.asBools
    val t0 = x(5) ^ x(21)
    val t1 = x(13) ^ x(29)
    val t2 = x(6) ^ x(22)
    val t3 = x(14) ^ x(30)
    val t4 = t0 ^ t1
    val t5 = x(3) ^ x(15)
    val t6 = x(7) ^ x(23)
    val t7 = x(19) ^ x(31)
    val t8 = x(2) ^ t3
    val t9 = x(10) ^ t2
    val t10 = x(0) ^ t4
    val t11 = x(1) ^ x(17)
    val t12 = x(4) ^ x(20)
    val t13 = x(8) ^ x(24)
    val t14 = x(9) ^ x(25)
    val t15 = x(11) ^ x(27)
    val t16 = x(12) ^ x(28)
    val t17 = x(18) ^ t8
    val t18 = x(26) ^ t9
    val t19 = t5 ^ t7
    val t20 = t6 ^ t15
    val t21 = x(16) ^ t10
    val t22 = t4 ^ t11
    val t23 = x(15) ^ x(31)
    val t24 = x(7) ^ t14
    val t25 = x(23) ^ t14
    val t26 = t2 ^ t13
    val t27 = t12 ^ t16
    val t28 = t19 ^ t20
    val t29 = t17 ^ t18
    val t30 = x(11) ^ t25
    val t31 = x(27) ^ t24
    val t32 = t3 ^ t21
    val t33 = x(0) ^ x(16)
    val t34 = x(7) ^ x(15)
    val t35 = x(8) ^ x(31)
    val t36 = x(1) ^ t3
    val t37 = x(26) ^ t5
    val t38 = t0 ^ t23
    val t39 = t1 ^ t6
    val t40 = t2 ^ t16
    val t41 = t3 ^ t12
    val t42 = t7 ^ t13
    val t43 = t11 ^ t15
    val t44 = t23 ^ t33
    val t45 = x(6) ^ t27
    val t46 = x(16) ^ t4
    val t47 = x(22) ^ t27
    val t48 = t0 ^ t20
    val t49 = t1 ^ t19
    val t50 = x(5) ^ t28
    val t51 = x(21) ^ t28
    val t52 = t5 ^ t22
    val t53 = t6 ^ t17
    val t54 = t7 ^ t22
    val t55 = t12 ^ t18
    val t56 = t16 ^ t17
    val t57 = t18 ^ t44
    val t58 = x(4) ^ t29
    val t59 = x(20) ^ t29
    val t60 = t21 ^ t37
    val y0 = (t46 ^ (t34 ^ t26))
    val y1 = (((x(8) ^ x(15)) ^ t25) ^ ((x(17) ^ t3) ^ t10))
    val y2 = (((x(9) ^ x(18)) ^ t36) ^ t57)
    val y3 = (t21 ^ (((x(2) ^ x(23)) ^ t9) ^ (t42 ^ t43)))
    val y4 = ((x(20) ^ t30) ^ (t52 ^ t56))
    val y5 = (((x(12) ^ x(21)) ^ t49) ^ t58)
    val y6 = (((x(13) ^ x(22)) ^ t41) ^ t50)
    val y7 = (t45 ^ ((x(14) ^ x(23)) ^ t38))
    val y8 = ((x(24) ^ (x(15) ^ x(23))) ^ t32)
    val y9 = (t22 ^ ((x(25) ^ t2) ^ (t35 ^ (x(16) ^ x(23)))))
    val y10 = ((t26 ^ (x(26) ^ (x(9) ^ x(17)))) ^ t53)
    val y11 = ((t31 ^ (t42 ^ (x(18) ^ (x(3) ^ x(10))))) ^ t32)
    val y12 = ((x(28) ^ t30) ^ (t54 ^ t55))
    val y13 = (((x(12) ^ x(29)) ^ t48) ^ t59)
    val y14 = (((x(13) ^ x(30)) ^ t40) ^ t51)
    val y15 = (t47 ^ ((x(14) ^ x(31)) ^ t39))
    val y16 = (t10 ^ ((x(23) ^ x(31)) ^ t26))
    val y17 = (((x(24) ^ x(31)) ^ t24) ^ (t36 ^ t46))
    val y18 = (((x(17) ^ x(25)) ^ t8) ^ t57)
    val y19 = ((t43 ^ ((x(7) ^ x(18)) ^ t26)) ^ t60)
    val y20 = ((x(4) ^ t31) ^ (t54 ^ t56))
    val y21 = (((x(5) ^ x(28)) ^ t49) ^ t59)
    val y22 = (((x(6) ^ x(29)) ^ t41) ^ t51)
    val y23 = (t47 ^ ((x(7) ^ x(30)) ^ t38))
    val y24 = ((x(7) ^ t35) ^ t32)
    val y25 = (t10 ^ ((t2 ^ t11) ^ (t34 ^ (x(9) ^ x(24)))))
    val y26 = ((t9 ^ (t13 ^ (x(1) ^ x(25)))) ^ t53)
    val y27 = ((t30 ^ (t8 ^ (x(19) ^ t13))) ^ t60)
    val y28 = ((x(12) ^ t31) ^ (t52 ^ t55))
    val y29 = (((x(13) ^ x(28)) ^ t48) ^ t58)
    val y30 = (((x(14) ^ x(29)) ^ t40) ^ t50)
    val y31 = (t45 ^ ((x(15) ^ x(30)) ^ t39))

    Cat(y31, y30, y29, y28, y27, y26, y25, y24,
        y23, y22, y21, y20, y19, y18, y17, y16,
        y15, y14, y13, y12, y11, y10, y9,  y8,
        y7,  y6,  y5,  y4,  y3,  y2,  y1,  y0)
  }

  def mixColumns64(value : Bits) = {
    val words = value.subdivideIn(32 bits)
    mixColumns32(words(1)) ## mixColumns32(words(0))
  }

  def invMixColumns64(value : Bits) = {
    val words = value.subdivideIn(32 bits)
    invMixColumns32(words(1)) ## invMixColumns32(words(0))
  }

  val logic = during setup new Logic {
    val ds = host[DecoderService]
    val dsRetainer = retains(ds.elaborationLock)

    awaitBuild()
    assert(Riscv.XLEN.get == 64)
    assert(writeBackAt >= readAt + 2)
    import SrcKeys._

    val wb = newWriteback(ifp, writeBackAt)
    add(Rvk.AES64_DE).srcs(SRC1.RF, SRC2.RF)
    add(Rvk.AES64IM).srcs(SRC1.RF)
    add(Rvk.AES64KS1I).srcs(SRC1.RF)
    uopRetainer.release()

    ds.addIllegalCheck { ctrl => False
    }
    dsRetainer.release()

    val onRead = new el.Execute(readAt) {
      val rs1 = up(el(IntRegFile, RS1)).asBits
      val rs2 = up(el(IntRegFile, RS2)).asBits

      val isDec = Bool()
      val reader = for(id <- 0 until Riscv.XLEN.get / 8) yield new Area {
        val byte = Bits(8 bits)
        val enable = SEL && (Decode.UOP(mapping.CRYPTO) || (id < 4).mux(Decode.UOP(mapping.KS), False))
        val address = (isDec ## byte).asUInt
      }

      val crypto = new Area {
        val rs1Bytes = rs1.subdivideIn(8 bits)
        val rs2Bytes = rs2.subdivideIn(8 bits)
        val decRow = invShiftRows(rs1Bytes, rs2Bytes)
        val encRow = shiftRows(rs1Bytes, rs2Bytes)

        isDec := Decode.UOP(mapping.DECRYPT)

        val data = Mux(isDec, decRow, encRow).subdivideIn(8 bits)
        data.zip(reader).foreach{case (data, read) => read.byte := data}
      }

      val ks1i = new Area {
        val SEL = insert(Decode.UOP(mapping.CHECK) === B"01")
        isDec clearWhen(SEL)
        val RCON = insert(Decode.UOP(mapping.RCON).asUInt)
        val rs = rs1.subdivideIn(32 bits)(1)
        val select = Mux(RCON === U(0xA, 4 bits), rs, rs.rotateRight(8))
        val value = select.subdivideIn(8 bits)
        when (SEL) {
          value.zipWithIndex.foreach{case (data, id) => reader(id).byte := data}
        }
      }

      val im = new Area {
        val SEL = insert(Decode.UOP(mapping.CHECK) === B"00")
        isDec clearWhen(SEL)
      }
    }

    val onData = new el.Execute(readAt + 2) {
      val romInitData = (AesZknPlugin.SBOX ++ AesZknPlugin.SBOX_INV).map(BigInt(_))
      val rom = onRead.reader.map(reader => new Area {
        val storage = Mem(Bits(8 bits), 512) initBigInt(romInitData)
        val enable = isReady && reader.enable
        val data = storage.readSync(reader.address, enable)
      })

      val romData = Cat(rom.map(_.data))
      val imData = up(el(IntRegFile, RS1)).asBits
      val data = onRead.im.SEL ? imData | romData

      /* Maybe we can add a new stage for the following? */
      val forward = mixColumns64(data)
      val inverse = invMixColumns64(data)
      val selector = Decode.UOP(mapping.DECRYPT) && Decode.UOP(mapping.CRYPTO) || onRead.im.SEL
      val mixed = Mux(selector, inverse, forward)

      val crypto = new Area {
        val result = Decode.UOP(mapping.MIDDLE_ROUND) ? mixed | data
      }

      val ks1i = new Area {
        val table = Vec(List(
          0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80,
          0x1b, 0x36, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        ).map(v => B(v, 8 bits)))

        val half = data.resize(32 bits) ^ table(onRead.ks1i.RCON).resize(32)
        val result = half ## half
      }

      val RESULT = insert(Mux(Decode.UOP(mapping.CRYPTO),
        crypto.result,
        Decode.UOP(mapping.KS) ? ks1i.result | mixed
      ))
    }

    val onWb = new el.Execute(writeBackAt) {
      wb.valid := SEL
      wb.payload := onData.RESULT
    }
  }
}

class Aes64Ks2ZknPlugin(
  val layer : LaneLayer,
  val readAt : Int = 0,
  val writeBackAt : Int = 1,
) extends ExecutionUnitElementSimple(layer){
  val logic = during setup new Logic {
    awaitBuild()
    assert(Riscv.XLEN.get == 64)
    import SrcKeys._

    val wb = newWriteback(ifp, writeBackAt)
    add(Rvk.AES64KS2).srcs(SRC1.RF, SRC2.RF)
    uopRetainer.release()

    assert(writeBackAt >= readAt)

    val onData = new el.Execute(readAt) {
      val rs1 = up(el(IntRegFile, RS1)).subdivideIn(32 bits)
      val rs2 = up(el(IntRegFile, RS2)).subdivideIn(32 bits)

      val low = rs1(1) ^ rs2(0)
      val high = low ^ rs2(1)

      val RESULT = insert(high ## low)
    }

    val onWb = new el.Execute(writeBackAt) {
      wb.valid := SEL
      wb.payload := onData.RESULT
    }
  }
}

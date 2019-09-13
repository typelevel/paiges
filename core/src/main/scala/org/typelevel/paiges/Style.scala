package org.typelevel.paiges

/**
 * Text styling for viewing in a terminal.
 *
 * This type represents foreground and background colors as well as
 * other text attributes. It uses a CSI sequence (ESC - '[') to start
 * a zero-width string containing one or more numeric codes, separated
 * by ; and ending in 'm'.
 *
 * Styles can be combined with ++; if two styles would conflict the
 * one on the right wins. For example: (Fg.Red ++ Fg.Green) = Fg.Green.
 *
 * See https://en.wikipedia.org/wiki/ANSI_escape_code#Escape_sequences
 * for more information about escape sequences.
 */
sealed abstract class Style extends Serializable { lhs =>
  def start: String
  def end: String = Style.Reset

  def ++(rhs: Style): Style = {
    val Style.Impl(fg0, bg0, sg0) = lhs
    val Style.Impl(fg1, bg1, sg1) = rhs
    Style.Impl(fg1 orElse fg0, bg1 orElse bg0, sg0 ::: sg1)
  }
}

object Style {

  /**
   * Represents neutral styling.
   *
   * Any other styles combined with Empty will apply their own
   * settings. (a ++ Empty) = a.
   */
  val Empty: Style = Impl(None, None, Nil)

  private def genCodes(ns: List[String]): String =
    ns.mkString("\u001b[", ";", "m")

  private case class Impl(fg: Option[String], bg: Option[String], sg: List[String]) extends Style {
    val start: String =
      if (fg.isEmpty && bg.isEmpty && sg.isEmpty) Reset
      else genCodes(fg.toList ::: bg.toList ::: sg.toList)
  }

  private val Reset: String = genCodes("0" :: Nil)

  /**
   * These escapes should be valid (although possibly not rendered) on
   * all ANSI-compatible terminals.
   *
   * See https://en.wikipedia.org/wiki/ANSI_escape_code#3/4_bit
   */
  object Ansi {

    /**
     * Styling attributes such as bold, italic, etc.
     */
    object Attr {

      private def code(n: Int): Style = Impl(None, None, n.toString :: Nil)

      val Bold = code(1)
      val Faint = code(2)
      val Italic = code(3)
      val Underline = code(4)
      val SlowBlink = code(5)
      val FastBlink = code(6)
      val Inverse = code(7)
      val Conceal = code(8)
      val CrossedOut = code(9)

      val BoldOff = code(21)
      val FaintOff = code(22)
      val ItalicOff = code(23)
      val UnderlineOff = code(24)
      val BlinkOff = code(25)
      val InverseOff = code(27)
      val ConcealOff = code(28)
      val CrossedOutOff = code(29)
    }

    /**
     * Foreground colors.
     */
    object Fg {

      private def code(n: Int): Style = Impl(Some(n.toString), None, Nil)

      val Black = code(30)
      val Red = code(31)
      val Green = code(32)
      val Yellow = code(33)
      val Blue = code(34)
      val Magenta = code(35)
      val Cyan = code(36)
      val White = code(37)
      val Default = code(39)

      val BrightBlack = code(90)
      val BrightRed = code(91)
      val BrightGreen = code(92)
      val BrightYellow = code(93)
      val BrightBlue = code(94)
      val BrightMagenta = code(95)
      val BrightCyan = code(96)
      val BrightWhite = code(97)
    }

    /**
     * Background colors.
     */
    object Bg {

      private def code(n: Int): Style = Impl(None, Some(n.toString), Nil)

      val Black = code(40)
      val Red = code(41)
      val Green = code(42)
      val Yellow = code(43)
      val Blue = code(44)
      val Magenta = code(45)
      val Cyan = code(46)
      val White = code(47)
      val Default = code(49)

      val BrightBlack = code(100)
      val BrightRed = code(101)
      val BrightGreen = code(102)
      val BrightYellow = code(103)
      val BrightBlue = code(104)
      val BrightMagenta = code(105)
      val BrightCyan = code(106)
      val BrightWhite = code(107)

    }
  }

  /**
   * Colors for modern XTerm and compatible terminals.
   *
   * See https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit
   */
  object XTerm {

    object Fg {

      def color(r: Double, g: Double, b: Double): Style = {
        // require(0.0 <= r && r < 1.0, s"red: $r")
        // require(0.0 <= g && g < 1.0, s"red: $r")
        // require(0.0 <= b && b < 1.0, s"red: $r")
        color((r * 6.0).toInt min 5, (g * 6.0).toInt min 5, (b * 6.0).toInt min 5)
      }

      /**
       * Uses a 6x6x6 color cube to render 8-bit colors.
       */
      def color(r: Int, g: Int, b: Int): Style = {
        require(0 <= r && r <= 5, s"invalid red: $r (should be 0-5)")
        require(0 <= g && g <= 5, s"invalid green: $g (should be 0-5)")
        require(0 <= b && b <= 5, s"invalid blue: $b (should be 0-5)")
        // 16 + 36 × r + 6 × g + b
        colorCode(16 + 36 * r + 6 * g + b)
      }

      def colorCode(code: Int): Style = {
        require(16 <= code && code <= 231)
        Impl(Some(s"38;5;$code"), None, Nil)
      }

      /**
       * Uses 24 steps to render 8-bit grayscale.
       */
      def gray(step: Int): Style = {
        require(0 <= step && step <= 23, s"invalid step: $step (should be 0-23)")
        val code = step + 232
        Impl(Some(s"38;5;$code"), None, Nil)
      }
    }

    object Bg {

      /**
       * Uses a 6x6x6 color cube to render 8-bit colors.
       */
      def color(r: Int, g: Int, b: Int): Style = {
        require(0 <= r && r <= 5, s"invalid red: $r (should be 0-5)")
        require(0 <= g && g <= 5, s"invalid green: $g (should be 0-5)")
        require(0 <= b && b <= 5, s"invalid blue: $b (should be 0-5)")
        // 16 + 36 × r + 6 × g + b
        val code = 16 + 36 * r + 6 * g + b
        Impl(None, Some(s"48;5;$code"), Nil)
      }

      /**
       * Uses 24 steps to render 8-bit grayscale.
       */
      def gray(step: Int): Style = {
        require(0 <= step && step <= 23, s"invalid step: $step (should be 0-23)")
        val code = step + 232
        Impl(None, Some(s"48;5;$code"), Nil)
      }
    }
  }
}

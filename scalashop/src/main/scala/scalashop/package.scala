
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  def inRange(src: Img, x: Int, y: Int): Boolean = {
    x >= 0 && x < src.width && y >= 0 && y < src.height
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    var redSum, greenSum, blueSum, alphaSum, surface = 0
    var xx = x - radius
    var yy = y - radius
    //println(xx, yy, "radius" ,radius)
    //println("width", src.width, "height", src.height)

    while (yy <= y + radius) {
      while (xx <= x + radius) {
        if (inRange(src, xx, yy)) {
          surface += 1
          redSum += red(src.apply(xx, yy))
          greenSum += green(src.apply(xx, yy))
          blueSum += blue(src.apply(xx, yy))
          alphaSum += alpha(src.apply(xx, yy))
        }
        //println(xx, yy)
        xx += 1
      }
      yy += 1
      xx = x - radius
    }

    assert(surface > 0)
    val redAvg = redSum / surface
    val greenAvg = greenSum / surface
    val blueAvg = blueSum / surface
    val alphaAvg = alphaSum / surface
    rgba(redAvg, greenAvg, blueAvg, alphaAvg)
  }

}

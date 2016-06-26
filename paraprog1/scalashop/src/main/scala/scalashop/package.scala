
import common._
import scala.collection.mutable.ListBuffer

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

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    
    def includePixel(x:Int, y:Int) : Boolean = clamp(x,0,src.width - 1) == x && clamp(y,0,src.height - 1) == y
    val components = Array(red _, green _, blue _, alpha _)
    def updateIfInBounds(sums : Array[Int], count : Int, x2Update : Int, y2Update : Int) : Int = {
      var retCount = count
      if (includePixel(x2Update, y2Update)) {
        var i = 0
        while (i<components.length) {
          sums(i) = sums(i)+ components(i)(src(x2Update,y2Update))
          i = i + 1
        }
        retCount = retCount + 1
      }
      retCount
    }
    
    var curRad = 1
    var retValue = src(x,y)
    var sums = Array(red(src(x,y)),green(src(x,y)),blue(src(x,y)),alpha(src(x,y)))
    var count = 1
    while (curRad<=radius) {
      var curX = x - curRad
      var curY = y - curRad
      
      
      while (curX < x + curRad) {        
        val mirrorCurX = x + (x - curX) 
        val mirrorCurY = y + (y - curY)
        count = updateIfInBounds(sums, count, curX, curY)
        count = updateIfInBounds(sums, count, mirrorCurX, mirrorCurY)
        curX = curX + 1
      }
      
      while (curY < y + curRad) {        
        val mirrorCurX = x + (x - curX) 
        val mirrorCurY = y + (y - curY)
        count = updateIfInBounds(sums, count, curX, curY)
        count = updateIfInBounds(sums, count, mirrorCurX, mirrorCurY)
        curY = curY + 1
      }
      retValue = rgba(sums(0)/count,sums(1)/count,sums(2)/count,sums(3)/count) 
      curRad = curRad + 1
    }  
    retValue
  }

}

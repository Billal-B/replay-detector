import java.io.File
import java.util

import org.bytedeco.javacpp.opencv_core.Mat
import org.bytedeco.javacpp.opencv_videoio._
import org.bytedeco.javacpp.opencv_imgcodecs._
import org.bytedeco.javacpp.opencv_imgproc._
import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.opencv_videoio.VideoCapture

object OpenCvUtils {
  
  /*
  * Find the contours of the input image (img).
  * The input image must be a binary image (for example, an image where the edges are detected).
  * Doesn't release the img matrix.
  * */
  def findContours(img: Mat): MatVector = {
    val contours = new MatVector()
    val hierarchy = new Mat()

    org.bytedeco.javacpp.opencv_imgproc.findContours(img, contours, hierarchy, RETR_LIST, CHAIN_APPROX_SIMPLE, new Point(0,0))
    hierarchy.release()
    
    contours
  }

  def saveFrames(capture: VideoCapture, frameIdxs: Seq[Int], folder:String): Unit = {
    // creating the folder
    if (! new File(folder).exists()) {
      new File(folder).mkdir()
    }
    frameIdxs.foreach{idx =>
      val frame = new Mat()
      capture.set(CAP_PROP_POS_FRAMES, idx)
      capture.read(frame)
      imwrite(folder + idx + ".png", frame)
    }
  }
  
  /*
  * Returns the HSV (Hue-Satured-Value) histogram of the input image (img).
  * This histogram is represented by 3 matrix of size binSize.
  * Doesn't release the img matrix.
  * */
  def getHSVhistogram(img: Mat, binSize: Int = 256): (Mat, Mat, Mat) = {
    val hsvImg = new Mat()
    cvtColor(img, hsvImg, COLOR_RGB2HSV)
    
    val vHist = new Mat(256, 1, CV_8UC1)
    val sHist = new Mat(256, 1, CV_8UC1)
    val hHist = new Mat(256, 1, CV_8UC1)
    val ranges = Array[Float](0f, 256f)
    val histSize = Array[Int](binSize)
    val channel0 = Array[Int](0)
    val channel1 = Array[Int](1)
    val channel2 = Array[Int](2)
    val mask = new Mat()

    calcHist(img, 1, channel0, mask, hHist, 1, histSize, ranges)
    calcHist(img, 1, channel1, mask, sHist, 1, histSize, ranges)
    calcHist(img, 1, channel2, mask, vHist, 1, histSize, ranges)

    /*
    calcHist(hsvImg, 1, channel0, mask, hHist, 45, binSize, ranges) // todo : use java default list instead of asJava + remove javaConverter deps
    calcHist(List(hsvImg).asJava, channel1, mask, sHist, histSize, ranges)
    calcHist(List(hsvImg).asJava, channel2, mask, vHist, histSize, ranges)

    ranges.release()
    histSize.release()
    hsvImg.release()
    channel0.release()
    channel1.release()
    channel2.release()
    */
    mask.release()
    hsvImg.release()
    
    (hHist, sHist, vHist)
  }
  
  /*
  * Crops the input image (img).
  * Doesn't release the img matrix.
  * TODO : fix this function, should be x,y, width, height
  * */
  def cropImage(
    img: Mat,
    left: Double = 0.1,
    right: Double = 0.9,
    top: Double = 0.1,
    bottom: Double = 0.9
  ): Mat = {
    val absLeft = (left * img.size().width).toInt
    val absRight = (right * img.size().width).toInt
    val absTop = (top * img.size().height).toInt
    val absBottom = (bottom * img.size().height).toInt
    val cropped = new Mat(img, new Rect(absLeft, absTop, absRight, absBottom))
    cropped
  }
  
  /*
  * Dilates the input image (img) => expand every non 0 pixel to the neighbooring pixels
  * Doesn't release the img matrix.
  * */
  def dilateImg(dilateWidth: Int, img: Mat): Unit = {
    val dilateKernel = new Mat(new Size(dilateWidth, dilateWidth), CV_8U, new Scalar(1.0))
    dilate(img, img, dilateKernel)
    dilateKernel.release()
  }
  
  /*
* Erodes the input image (img) => set to 0 every pixel with no neighboor != 0
* Doesn't release the img matrix.
* */
  def erodeImg(erodeWidth: Int, img: Mat): Unit = {
    val erodeKernel = new Mat(new Size(erodeWidth, erodeWidth), CV_8U, new Scalar(1.0))
    erode(img, img, erodeKernel)
    erodeKernel.release()
  }
  
  
  /*
  * Returns a binary image of the edges in the input image (img).
  * The binary image is obtained by applying a blur on the original image
  * and then applying the Canny edge detection algorithm.
  * Doesn't release the img matrix.
  * */
  def doCannyEdgeDetection(img: Mat): Mat = {
    val edges = new Mat()
    
    // blurring
    val blurred = new Mat()
    //val gaussian = new Mat()
    GaussianBlur(img, blurred, new Size(7,7), 0)
    //Imgproc.bilateralFilter(img, blurred, 5, 5,5, Core.BORDER_DEFAULT)
    //Imgproc.medianBlur(img, blurred, 7)
    // canny edge detection
    Canny(blurred, edges, 200d, 100d, 3, true)
    
    blurred.release()
    //gaussian.release()
    edges
  }
  
  /*
* Returns a binary image that is only made of the contours in the input image (img).
* Only keeps the contours that are at least of length minContourLength.
* Doesn't release the img matrix.
* */
  def makeCountoursFrame(img: Mat, minContourLength: Double = 0.0): Mat = {
    val edges = doCannyEdgeDetection(img)
    val hierarchy = new Mat()
    val contoursFrame = Mat.zeros(img.size(), CV_8U).asMat()
    val contours = new MatVector(findContours(edges).get().filter {contour => arcLength(contour, false) > minContourLength}:_*)
    drawContours(contoursFrame, contours, -1, new Scalar(255.0))
    threshold(contoursFrame, contoursFrame, 75d, 255d, THRESH_BINARY)
    contours.get().foreach(_.release())
    hierarchy.release()
    edges.release()
    contoursFrame
  }
  
  
  def blurAndThresh(img: Mat) = {
    val blurred = new Mat()
    /*
    Imgproc.pyrUp(img, blurred)
    for (i <- 0 to 5) {
      Imgproc.medianBlur(blurred, blurred, 5)
    }
    Imgproc.pyrDown(blurred, blurred)
    */
    GaussianBlur(img, blurred, new Size(3,3), 0)
    //val filtered = new Mat()
    //Imgproc.bilateralFilter(blurred, filtered, 15, 80,80, Core.BORDER_DEFAULT)
    threshold(blurred, blurred, 0d, 255d, THRESH_BINARY + THRESH_OTSU) // todo : remove otsu ?
    blurred.copyTo(img)
    blurred.release()
    //filtered.release()
  }
}

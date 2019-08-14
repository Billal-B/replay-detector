import java.io.File
import java.util

import scala.collection.JavaConverters._
import org.opencv.videoio.Videoio._
import org.opencv.imgcodecs.Imgcodecs._
import org.opencv.imgproc.Imgproc._
import org.opencv.core.Core._
import org.opencv.core.CvType._
import org.opencv.core.{Core, CvType, Mat, MatOfFloat, MatOfInt, MatOfPoint, MatOfPoint2f, Point, Scalar, Size}
import org.opencv.videoio.VideoCapture
import org.opencv.video.Video.calcOpticalFlowFarneback

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

object OpenCvUtils {

  /*
  * Find the contours of the input image (img).
  * The input image must be a binary image (for example, an image where the edges are detected).
  * Doesn't release the img matrix.
  * */
  def findContours(img: Mat): util.List[MatOfPoint] = {
    val contours = new java.util.Vector[MatOfPoint]()
    val hierarchy = new Mat()

    org.opencv.imgproc.Imgproc.findContours(img, contours, hierarchy, RETR_LIST, CHAIN_APPROX_SIMPLE, new Point(0,0))
    hierarchy.release()

    contours
  }

  def saveFrames(capture: VideoCapture, frameIdxs: Seq[Int], folder:String, tag: Option[String] = None, saveSize: Int = 0): Unit = {
    // creating the folder
    if (! new File(folder).exists()) {
      new File(folder).mkdir()
    }
    val logoFolder = new File(folder)
    // creating the subfolder
    new File(logoFolder.getCanonicalPath + "/" + tag.getOrElse("no_tag")).mkdir()
    val logoSubfolder = new File(logoFolder.getCanonicalPath + "/" + tag.getOrElse("no_tag"))

    frameIdxs.foreach{idx =>
      new File(logoSubfolder.getCanonicalPath + "/" + idx).mkdir()
      val indexDir = new File(logoSubfolder.getCanonicalPath + "/" + + idx)

      var prevFrame = new Mat()
      capture.set(CAP_PROP_POS_FRAMES, idx - (saveSize / 2).toInt + 0)
      capture.read(prevFrame)
      imwrite(indexDir.getCanonicalPath + "/" + "0" +".png", prevFrame)

      val framesToSave = (1 to saveSize).map {i =>
        val frame = new Mat()
        capture.set(CAP_PROP_POS_FRAMES, idx - (saveSize / 2).toInt + i)
        capture.read(frame)
        frame
      }.toVector

      if (framesToSave.length > 2) calcOpticalFlow(framesToSave, indexDir.getCanonicalPath + "/")

      framesToSave.zipWithIndex.foreach{case (frame, i) =>
        imwrite(indexDir.getCanonicalPath + "/" + i +".png", frame)
      }

      framesToSave.foreach(_.release())
    }
  }

  private def calcOpticalFlow(frames: Vector[Mat], directory: String): Unit = {
    val matInit = new Mat(frames.head.rows, frames.head.cols(), CvType.CV_32FC2, new Scalar(0,0,0))
    val accumulatedFlow = new Mat(frames.head.rows, frames.head.cols(), CvType.CV_32FC2)
    val (trajectoryFlow, accDist) =
      0.until(frames.length - 1).foldLeft(matInit, 0d) { case ((prevFlow, prevAccDist), idx) =>
        val prevFrame = frames(idx)
        val currentFrame = frames(idx + 1)
        val flow = new Mat()

        val grayedPrev = new Mat()
        cvtColor(prevFrame, grayedPrev, COLOR_RGB2GRAY)
        val grayedCurrent = new Mat()
        cvtColor(currentFrame, grayedCurrent, COLOR_RGB2GRAY)

        calcOpticalFlowFarneback(grayedPrev, grayedCurrent, flow, 0.5, 3, 15, 3, 5, 1.2, 0)

        val accflow = new Mat(frames.head.rows, frames.head.cols(), CvType.CV_32FC2, new Scalar(0,0,0))
        Core.add(prevFlow, flow, accflow)

        // calculing the norm of the optical flux vector
        // see : https://hal.inria.fr/inria-00583818/document, equation 2
        val norm = Core.norm(flow)

        // storing the current flow to be referenced by its index and later generates a 2L image channel
        // with the flow infos
        // See : https://papers.nips.cc/paper/5353-two-stream-convolutional-networks-for-action-recognition-in-videos.pdf
        val cflow = new Mat(frames.head.rows, frames.head.cols(), frames.head.`type`(), new Scalar(0,0,0))
        val cflowHorizontal = new Mat(frames.head.rows, frames.head.cols(), frames.head.`type`(), new Scalar(0,0,0))
        val cflowVertical = new Mat(frames.head.rows, frames.head.cols(), frames.head.`type`(), new Scalar(0,0,0))

        val step = 5
        for {
          y <- 0 until cflowHorizontal.rows() by step
          x <- 0 until cflowHorizontal.cols() by step
        } {
          // storing the current flow to be referenced by its index and later generates a 2L image channel
          // with the flow infos
          // See : https://papers.nips.cc/paper/5353-two-stream-convolutional-networks-for-action-recognition-in-videos.pdf
          val point = flow.get(y, x)
          line(cflowHorizontal, new Point(x, y), new Point(Math.round(x.toDouble), Math.round(y.toDouble+point(1))),
            new Scalar(255d, 255d, 255d))
          line(cflowVertical, new Point(x, y), new Point(Math.round(x.toDouble + point(0)), Math.round(y.toDouble)),
            new Scalar(255d, 255d, 255d))
          line(cflow, new Point(x, y), new Point(Math.round(x.toDouble + point(0)), Math.round(y.toDouble+point(1))),
            new Scalar(255d, 255d, 255d))
        }
        imwrite(directory + "flow2D_" + (idx * 2) + ".png", cflowHorizontal)
        imwrite(directory + "flow2D_" + (idx * 2 + 1) + ".png", cflowVertical)
        imwrite(directory + "flow3D_" + idx + ".png", cflow)

        cflowHorizontal.release()
        cflowVertical.release()
        cflow.release()
        grayedPrev.release()
        grayedCurrent.release()
        flow.release()
        prevFlow.release()
        (accflow, norm + prevAccDist)
      }

    val cflow = new Mat(frames.head.rows, frames.head.cols(), frames.head.`type`(), new Scalar(0,0,0))

    val normalizedFlow = new Mat(trajectoryFlow.rows,trajectoryFlow.cols(), trajectoryFlow.`type`(), new Scalar(0))
    Core.divide(trajectoryFlow, new Scalar(accDist, accDist), normalizedFlow)

    val step = 5
    for {
      y <- 0 until cflow.rows() by step
      x <- 0 until cflow.cols() by step
    } {
      // storing the current flow to be referenced by its index and later generates a 2L image channel
      // with the flow infos
      // See : https://papers.nips.cc/paper/5353-two-stream-convolutional-networks-for-action-recognition-in-videos.pdf
      val point = trajectoryFlow.get(y, x)
      line(cflow, new Point(x, y), new Point(Math.round(x.toDouble + point(0)), Math.round(y.toDouble+point(1))),
        new Scalar(255d, 255d, 255d))
    }
    imwrite(directory + "flow.png", cflow)

    cflow.release()
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
    val ranges = new MatOfFloat(0f, 256f)
    val histSize = new MatOfInt(binSize)
    val channel0 = new MatOfInt(0)
    val channel1 = new MatOfInt(1)
    val channel2 = new MatOfInt(2)
    val mask = new Mat()

    calcHist(List(img).asJava, channel0, mask, hHist, histSize, ranges)
    calcHist(List(img).asJava, channel1, mask, sHist, histSize, ranges)
    calcHist(List(img).asJava, channel2, mask, vHist, histSize, ranges)

    //calcHist(img, 1, channel1, mask, sHist, 1, histSize, ranges)
    //calcHist(img, 1, channel2, mask, vHist, 1, histSize, ranges)

    /*
    calcHist(hsvImg, 1, channel0, mask, hHist, 45, binSize, ranges) // todo : use java default list instead of asJava + remove javaConverter deps
    calcHist(List(hsvImg).asJava, channel1, mask, sHist, histSize, ranges)
    calcHist(List(hsvImg).asJava, channel2, mask, vHist, histSize, ranges)
*/
    ranges.release()
    histSize.release()
    hsvImg.release()
    channel0.release()
    channel1.release()
    channel2.release()

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
    val cropped = new Mat(img, new org.opencv.core.Rect(absLeft, absTop, absRight, absBottom))
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
    Try {
      val edges = doCannyEdgeDetection(img)
      val hierarchy = new Mat()
      val contoursFrame = Mat.zeros(img.size(), CV_8U)

      val contours = findContours(edges).asScala.filter { _contour => val contour = new MatOfPoint2f(); _contour.convertTo(contour, CV_32F); arcLength(contour, false) > minContourLength }.asJava

      //val contours = new MatVector(findContours(edges)..get().filter { contour => arcLength(contour, false) > minContourLength }: _*)
      drawContours(contoursFrame, contours, -1, new Scalar(255.0))
      threshold(contoursFrame, contoursFrame, 75d, 255d, THRESH_BINARY)

      contours.forEach(_.release())
      hierarchy.release()
      edges.release()
      contoursFrame
    } match {
      case Success(m: Mat) =>
        m
      case Failure(ex) =>
        ex.printStackTrace()
        throw new Exception("Error while drawing countours")
    }
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

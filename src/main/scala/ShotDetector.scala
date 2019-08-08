import org.opencv.videoio.Videoio._
import org.opencv.imgcodecs.Imgcodecs._
import org.opencv.imgproc.Imgproc._
import org.opencv.core.Core._
import org.opencv.core.CvType._
import org.opencv.core.{Mat, Point, Scalar, Size}
import org.opencv.videoio.VideoCapture
import scala.collection.JavaConverters._

/*
* Handles the segmentation of videos into shots
* */
class ShotDetector(capture: VideoCapture) extends Configuration {
  import OpenCvUtils._
  import ShotDetector._

  private val fps: Int = math.round(capture.get(CAP_PROP_FPS)).toInt
  capture.set(CAP_PROP_POS_FRAMES, startFrame)

  // shot detection parameters
  private val windowSize: Int = 5
  private val threshold = 0.25
  private val binSize = 256

  // video parameters
  private val frameNumber: Int = math.min(capture.get(CAP_PROP_FRAME_COUNT).toInt - startFrame, frameToAnalyse) - windowSize

  println(s"Video is at $fps fps, analyzing $frameNumber frames")
  println(s"Video size : $videoHeight x $videoWidth")
  println(s"Window size : $windowSize")

  /*
  * Finds the shots in the video given its transition frames.
  * */
  private def findShots(transitionFrames: Vector[TransitionFrame]) = {
    transitionFrames
      .filter(_.rank != 1) // get only the frame which are potential transition frames (ie, rank > 1)
      .groupBy(tf => math.round(tf.index / (fps/2))) // groups in windows of 0.5 seconds to avoid potential false positives
      .map{ case (_, frame) => frame.minBy(_.index).index}.toVector // select in each group the frame with the lowest index
  }

  /*
  * Computes the rank tracing for an image.
  * The image is represented by its HSV histogram and the histogram of the previous frames (param : windowSizewindowSize).
  * See : ONLINE, SIMULTANEOUS SHOT BOUNDARY DETECTION AND KEY FRAME EXTRACTION FOR SPORTS VIDEOS USING RANK TRACING
  * */
  private def computeRankTracing(histograms: Seq[(Mat, Mat, Mat)]): Int = {

    val xt = new Mat()
    val w = new Mat()
    val u = new Mat()
    val vt = new Mat()

    histograms.foreach {
      case (hHist, sHist, vHist) =>
        val hists = new Mat()
        hconcat(List(hHist, sHist, vHist).asJava, hists) // todo : use java list instead of asJava
        xt.push_back(hists.reshape(0, 1))
    }
    SVDecomp(xt, w, u, vt)

    val bestSvalue = w.get(0,0)(0)
    val res = (0 until windowSize).foldLeft(0) {
      case (tot, idx) =>
        val sValue = w.get(idx, 0)(0)
        if (sValue / bestSvalue > threshold) tot + 1
        else tot
    }

    xt.release()
    w.release()
    u.release()
    vt.release()
    res
  }


  /*
  * Finds the frame that makes the transition between shots in the video.
  * */
  private def computeTransitionFrames: Vector[TransitionFrame] = {
    var histWindow = (0 until windowSize).map { _ =>
      val frame = new Mat()
      capture.read(frame)

      val hist = getHSVhistogram(frame)
      frame.release()
      hist
    }

    val _videoWidth = videoWidth
    val _videoHeight = videoHeight
    val _startFrame = startFrame
    var time = System.currentTimeMillis()
    val windows = collection.mutable.ListBuffer(TransitionFrame(windowSize + frameNumber - 1, computeRankTracing(histWindow)))

    for (i <- 0 until frameNumber - windowSize) {
      if (i % 50000 == 0 & i != 0) println("Processed " + i + "\tTime : " + ((System.currentTimeMillis() - time) / 1000).toInt)
      val frame = new Mat()
      capture.read(frame)
      val resized = new Mat()
      resize(frame, resized, new Size(_videoWidth, _videoHeight))

      val currentHist = getHSVhistogram(resized)
      histWindow.head._1.release()
      histWindow.head._2.release()
      histWindow.head._3.release()
      histWindow = histWindow.tail :+ currentHist
      val rankTracing = computeRankTracing(histWindow)

      frame.release()
      resized.release()
      windows.+=:(TransitionFrame(i + _startFrame, rankTracing))
    }
    windows.toVector
  }

  /* Returns the shots in the video */
  def findShot: Vector[Int] = {
    val transitionFrame = computeTransitionFrames
    findShots(transitionFrame)
  }
}

object ShotDetector {
  case class TransitionFrame(index: Int, rank: Int)
}

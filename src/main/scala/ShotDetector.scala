import org.opencv.core._
import org.opencv.imgproc.Imgproc
import org.opencv.videoio.{VideoCapture, Videoio}
import org.opencv.core.Core

import scala.collection.JavaConverters._

case class HSVhistogram(hHist: Mat, sHist: Mat, vHist:Mat)
case class SlidingWindow(index: Int, hsvHistograms: Seq[HSVhistogram], rank: Int)

private class ShotDetector(
  videoPath: String,
  frameToAnalyse: Int = Int.MaxValue,
  startFrame: Int = 0,
  display: Boolean = false
) {
  System.loadLibrary(Core.NATIVE_LIBRARY_NAME)
  private val (width, height) = (224,224)
  private val capture = new VideoCapture(videoPath)
  capture.set(Videoio.CAP_PROP_POS_FRAMES, startFrame)
  private val fps: Double = capture.get(Videoio.CAP_PROP_FPS).toInt
  private val windowSize: Int = 10
  private val frameNumber: Int = math.min(capture.get(Videoio.CAP_PROP_FRAME_COUNT).toInt, frameToAnalyse) - windowSize
  private val threshold = 0.25
  private val minShotLengthInSecond = 4
  private val binSize = 256
  
  println(s"Video is at $fps fps, analyzing $frameNumber frames")
  
  
  def findShots = {
    val slidingWindows = computeSlidingWindows

    val windows = computeKeyFrame(slidingWindows)
    windows.foreach{ case (keyFrame, endingFrame) =>
      val keyTime = startFrame + keyFrame.index
      val endTime = startFrame + endingFrame.index
      print(s"Key Time : ${(keyTime/fps/60).toInt}:${(keyTime/fps%60).toInt}\tKey Rank : ${keyFrame.rank}")
      println(s"\tEnd Time : ${(endTime/fps/60).toInt}:${(endTime/fps%60).toInt}\tEnd Rank : ${endingFrame.rank}")
    }
    println("Found shots !")
    capture.release()
  }
  
  
  private def computeKeyFrame(slidingWindows: Seq[SlidingWindow]) = {
    val (_, _, endingFrame, _ , keysFrames) = slidingWindows
      //.grouped(windowSize).map(_.maxBy(_.rank)) // get the max rank for each window
      .foldLeft(1,0, Seq.empty[SlidingWindow], Seq.empty[SlidingWindow], Seq.empty[SlidingWindow]){
      case ((prevRank, prevIndex, allEndingFrames, currentShot, allKeyFrames), currentFrame) =>
        if (prevRank > currentFrame.rank && currentFrame.rank == 1) {
          val keyframe: SlidingWindow = currentShot.maxBy(_.rank)
          (currentFrame.rank, currentFrame.index, allEndingFrames :+ currentFrame, Seq.empty[SlidingWindow], allKeyFrames :+ keyframe)
        }
        else (currentFrame.rank, currentFrame.index, allEndingFrames, currentShot :+ currentFrame, allKeyFrames)
    }
    keysFrames.zip(endingFrame)
  }
  
  
  private def computeRankTracing(histograms: Seq[HSVhistogram]): Int = {
    val xt = new Mat()
    histograms.zipWithIndex.foreach {
      case (HSVhistogram(hHist, sHist, vHist), idx) =>
        val hists = new Mat()
        Core.hconcat(List(hHist, sHist, vHist).asJava, hists)
        xt.push_back(hists.reshape(0, 1))
    }
    val w = new Mat()
    val u = new Mat()
    val vt = new Mat()
    
    Core.SVDecomp(xt, w, u, vt)
    val bestSvalue = w.get(0,0)(0)
    (0 until windowSize).foldLeft(0) {
      case (tot, idx) =>
        val sValue = w.get(idx, 0)(0)
        if (sValue / bestSvalue > threshold) tot + 1
        else tot
    }
  }
  
  
  private def computeSlidingWindows= {
    val firstHists = (0 until windowSize).foldLeft(Seq.empty[HSVhistogram]) {case (hist, index) =>
      val frame = new Mat()
      capture.read(frame)
      
      hist :+ getHSVhistogram(frame)
    }
    
    val firstWindow = SlidingWindow(windowSize - 1, firstHists, computeRankTracing(firstHists))
    
    (windowSize until frameNumber).foldLeft(Seq(firstWindow)){
      case (allSlidingWindows, index) =>
        val prevWindow = allSlidingWindows.last.hsvHistograms.tail
        val frame = new Mat()
        capture.read(frame)
        
        Imgproc.resize(frame, frame, new Size(width, height))
        val currentHist = getHSVhistogram(frame)
        val currentWindow = prevWindow :+ currentHist
        val rankTracing = computeRankTracing(currentWindow)
  
        val hsvImg = new Mat()
        Imgproc.cvtColor(frame, hsvImg, Imgproc.COLOR_RGB2HSV)
        if (display) {
          if (rankTracing > 1) {
            println(s"Time : ${(index/fps/60).toInt}:${(index/fps%60).toInt} " +
              s" \tRank :$rankTracing"
            )
            org.opencv.highgui.HighGui.imshow("tmp", frame)
            org.opencv.highgui.HighGui.waitKey(0)
          }
          else {
            org.opencv.highgui.HighGui.imshow("tmp", frame)
            org.opencv.highgui.HighGui.waitKey(1)
          }
          

        }

        allSlidingWindows :+ SlidingWindow(index, currentWindow, rankTracing)
    }
  }
  
  
  private def getHSVhistogram(img: Mat): HSVhistogram = {
    val hsvImg = new Mat()
    Imgproc.cvtColor(img, hsvImg, Imgproc.COLOR_RGB2HSV)
    
    val vHist = new Mat(256, 1, CvType.CV_8UC1)
    val sHist = new Mat(256, 1, CvType.CV_8UC1)
    val hHist = new Mat(256, 1, CvType.CV_8UC1)
    val channels = new MatOfInt(0)
    val ranges = new MatOfFloat(0f, 256f)
    val histSize = new MatOfInt(binSize)
    
    Imgproc.calcHist(List(hsvImg).asJava, new MatOfInt(0), new Mat(), hHist, histSize, ranges)
    Imgproc.calcHist(List(hsvImg).asJava, new MatOfInt(1), new Mat(), sHist, histSize, ranges)
    Imgproc.calcHist(List(hsvImg).asJava, new MatOfInt(2), new Mat(), vHist, histSize, ranges)
    
    HSVhistogram(hHist, sHist, vHist)
  }
}


object ShotDetector{
  def apply(
    videoPath: String,
    frameToAnalyse: Int = Int.MaxValue,
    startFrame: Int = 0,
    display: Boolean = false
  ) = {
    val shotDetector = new ShotDetector(videoPath, frameToAnalyse, startFrame, display = display)
    shotDetector.findShots
  }
}
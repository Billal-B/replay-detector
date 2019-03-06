import java.util

import org.opencv.core.{Core, _}
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc
import org.opencv.videoio.{VideoCapture, Videoio}

import scala.collection.JavaConverters._

case class HSVhistogram(hHist: Mat, sHist: Mat, vHist:Mat) {
  def release(): Unit = {
    hHist.release()
    sHist.release()
    vHist.release()
  }
}

case class SlidingWindow(index: Int, rank: Int)


trait Releasable[A] {
  def map[B](f: A => B): B
  def flatMap[B](f: A => Releasable[B]): Releasable[B]
}

object ReleasableMat {
  
  class ReleasableMat(m: Mat) extends Releasable[Mat] {
    override def map[B](f: Mat => B): B = {
      val res = f(m)
      m.release()
      res
    }
    
    override def flatMap[B](f: Mat => Releasable[B]): Releasable[B] = {
      val res = f(m)
      m.release()
      res
    }
  }
  
  def executeAndReleaseMat[A](mat: Mat)(f: Mat => A): A = {
    val res = f(mat)
    mat.release()
    res
  }
  
  def executeAndReleaseMat[A](mats: List[Mat])(f: List[Mat] => A): A = {
    val res = f(mats)
    mats.foreach(m => m.release())
    res
  }
}

object AutoMatReleaseUtils {
  import scala.collection.immutable.Vector
  import scala.collection.mutable.Buffer
  import scala.util.DynamicVariable
  private val scopes = new DynamicVariable[Vector[Buffer[Mat]]](Vector.empty)
  
  private def currentScope = scopes.value.lastOption.getOrElse(sys.error("auto: no current scope!"))
  
  def withAuto[T](thunk: => T): T = {
    scopes.withValue(scopes.value :+ Buffer.empty){
      val res = thunk
      currentScope.foreach(_.release())
      res
    }
  }
  
  def auto[T](mat: Mat): Mat = {
    currentScope += mat
    mat
  }
}

private class ShotDetector(
  videoPath: String,
  frameToAnalyse: Int = Int.MaxValue,
  startFrame: Int = 0,
  display: Boolean = false
) {
  System.loadLibrary(Core.NATIVE_LIBRARY_NAME)
  private val capture = new VideoCapture(videoPath)
  capture.set(Videoio.CAP_PROP_POS_FRAMES, startFrame)
  // shot detection parameters
  private val windowSize: Int = 5
  private val threshold = 0.25
  private val binSize = 256
  
  // video parameters
  private val frameNumber: Int = math.min(capture.get(Videoio.CAP_PROP_FRAME_COUNT).toInt - startFrame, frameToAnalyse) - windowSize
  private val fps: Double = capture.get(Videoio.CAP_PROP_FPS)
  private val videoWidth = 100
  private val videoHeight = 100
  
  // logo detection parameters
  private val mosaicBorderSize = 2
  private val (mosaicWidth, mosaicHeight) = (0.64 * videoWidth + mosaicBorderSize * 2, 0.64 * videoHeight + mosaicBorderSize * 2) // todo : check  "+ mosaicBorderSize * 2"
  private val saveWindowSize = math.min((fps / 6).toInt, 5)
  private val minLogoSegmentLength = mosaicHeight / 4
  private val logoThreshold = saveWindowSize * ((mosaicHeight + mosaicWidth) / 2) / 6
  private val contourDiffDilate: Option[Int] = None
  
  
  println("Video : " + videoPath)
  println(s"Video is at $fps fps, analyzing $frameNumber frames")
  println(s"Window size : $windowSize")
  println(s"Save window size : $saveWindowSize")
  println("Logo threshold : " + logoThreshold)
  println("Min logo segment length : " + minLogoSegmentLength)
  

  def findShotFrames: Seq[Int] = {
    val t = System.currentTimeMillis()
    
    val slidingWindows = computeSlidingWindows
    val computeSlidingWindowsTime = System.currentTimeMillis() - t
    println("Time to find shots: " + computeSlidingWindowsTime)
    
    val foundShot = computeKeyFrame(slidingWindows)
    saveSlidingWindows(foundShot)
    val saveShotTime = System.currentTimeMillis() - t - computeSlidingWindowsTime
    println("Time to save shot: " + saveShotTime)
    
    val logosTemplates = findLogoTemplates(foundShot)
    logosTemplates.sortBy(_.index).foreach{ l =>
      println(s"Replay at ${l.index} " +
        s" from ${(l.index/fps/60).toInt}m ${(l.index/fps%60).toInt}s" +
        s" to ${(l.matches/fps/60).toInt}m ${(l.matches/fps%60).toInt}s")
    }
    val computeLogoTemplateTime = System.currentTimeMillis() - saveShotTime - t
    println("Time to find logo : " + computeLogoTemplateTime)
    
    val replays = findReplay(logosTemplates)
    val computeLogoTime = System.currentTimeMillis() - computeLogoTemplateTime - t
    println("Time to filter logo : " + computeLogoTime)
    replays.sortBy(_.index).foreach { r =>
      println(s"Replay at ${r.index} " +
        s" from ${(r.index/fps/60).toInt}m ${(r.index/fps%60).toInt}s" +
        s" to ${(r.matches/fps/60).toInt}m ${(r.matches/fps%60).toInt}s")
    }
  
    capture.release()
    foundShot
  }
  
  
  private def computeKeyFrame(slidingWindows: Seq[SlidingWindow]) = {
    val (_, _, endingFrames, _) = slidingWindows
      //.grouped(windowSize).map(_.maxBy(_.rank)) // get the max rank for each window
      .foldLeft(1,0, Seq.empty[Int], Seq.empty[SlidingWindow]){
      case ((prevRank, prevIndex, allEndingFrames, currentShot), currentFrame) =>
        if (prevRank > currentFrame.rank && currentFrame.rank == 1) {
          val keyframe = currentShot.filter(_.index > 1).minBy(_.index)
          (currentFrame.rank, currentFrame.index, allEndingFrames :+ keyframe.index, Seq.empty[SlidingWindow])
        }
        else (currentFrame.rank, currentFrame.index, allEndingFrames, currentShot :+ currentFrame)
    }
    endingFrames
  }
  
  
  
  private def computeRankTracing(histograms: Seq[HSVhistogram]): Int = {
    
    val xt = new Mat()
    val w = new Mat()
    val u = new Mat()
    val vt = new Mat()
    
    histograms.foreach {
      case (HSVhistogram(hHist, sHist, vHist)) =>
        val hists = new Mat()
        Core.hconcat(List(hHist, sHist, vHist).asJava, hists)
        xt.push_back(hists.reshape(0, 1))
    }
    Core.SVDecomp(xt, w, u, vt)
    
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
  
  private def computeSlidingWindows= {
    var histWindow = (0 until windowSize).map { _ =>
        val frame = new Mat()
        capture.read(frame)
        
        val hist = getHSVhistogram(frame)
        frame.release()
        hist
    }
    var i = 0
    var time = System.currentTimeMillis()
    val windows = collection.mutable.ListBuffer(SlidingWindow(windowSize + frameNumber - 1, computeRankTracing(histWindow))) // todo : try ListBuffer instead of ArrayBuffer
    
  
    for (i <- 0 until frameNumber - windowSize) {
      if (i % 50000 == 0 & i != 0) println("Processed " + i + "\tTime : " + ((System.currentTimeMillis() - time) / 1000).toInt)
      val frame = new Mat()
      capture.read(frame)
      val resized = new Mat()
      Imgproc.resize(frame, resized, new Size(videoWidth, videoHeight))
      
      val currentHist = getHSVhistogram(resized) // most of the complexity is here
      histWindow.head.release()
      histWindow = histWindow.tail :+ currentHist
      val rankTracing = computeRankTracing(histWindow)
      if (display) {
        if (rankTracing > 1) {
          println(s"Time : ${(i / fps / 60).toInt}:${(i / fps % 60).toInt} " +
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
      frame.release()
      resized.release()
      windows.+=:(SlidingWindow(i + startFrame, rankTracing))
    }
    windows
  }
  
  // todo : fix, it seems to be O(n²)
  private def contourDiff(imgA: Mat, imgB: Mat) = {
    
    val diff = new Mat()
    Core.bitwise_and(imgA, imgB, diff)
    
    contourDiffDilate.foreach {dilateWidth =>
      val dilateKernel = new Mat(new Size(dilateWidth, dilateWidth), CvType.CV_8U, new Scalar(1.0))
      Imgproc.dilate(diff, diff, dilateKernel)
      dilateKernel.release()
    }

  
    val contours = new util.ArrayList[MatOfPoint]()
    val hierarchy = new Mat()
    
    Imgproc.findContours(diff, contours, hierarchy, Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE, new org.opencv.core.Point(0,0))
    
    val score = collection.mutable.Map.empty[(Int, Int), Double] // todo: find better data structure than Map
    val totScore = collection.mutable.Map.empty[Int, Double] // todo: find better data structure than Map
    
    for (ct <- contours.asScala if ct.total() > minLogoSegmentLength) {
      val x = (ct.get(0,0)(1) / mosaicWidth).toInt
      val y = (ct.get(0,0)(0) / mosaicHeight).toInt // todo : check that (1) index is indeed the height
      val dst = new org.opencv.core.MatOfPoint2f()
      ct.convertTo(dst, org.opencv.core.CvType.CV_32F)
      val r = score.getOrElse((x,y), 0d) + Imgproc.arcLength(dst, false)
      dst.release()
      score.update((x,y), r)
      totScore.update(x, totScore.getOrElse(x, 0d) + r)
    }
    
    if (totScore.nonEmpty) totScore.maxBy(_._2)._2
    else 0d
  }
  
  
  private def findReplay(logoTemplates: Seq[Logo]) = {
    val possibleLogoFrames = collection.mutable.ArrayBuffer.empty[Logo]
    val startLogoIdxPool = logoTemplates.groupBy(_.index)
    val endLogoIdxPool = logoTemplates.groupBy(_.matches)
    for ((startLogoIdx, matchingFrames)<- startLogoIdxPool) {
      val startLogoFile = startLogoIdx + "A.png"
      val startLogoShot = Imgcodecs.imread("shot/" + startLogoFile, Imgcodecs.IMREAD_UNCHANGED)
      var matched = 0
      for ((endLogoIdx, endLogo) <- endLogoIdxPool)
           if (math.abs(endLogoIdx - startLogoIdx) > fps * 2 & matched <= 2)
      {
        val endLogoFile = endLogoIdx + "B.png"
        val endLogoShot = Imgcodecs.imread("shot/" + endLogoFile, Imgcodecs.IMREAD_UNCHANGED)
        val res = contourDiff(startLogoShot, endLogoShot)
        if (res > logoThreshold * 2) matched += 1
        endLogoShot.release()
      }
      if (matched > 2) {
        possibleLogoFrames += matchingFrames.minBy(logo => logo.matches - startLogoIdx)
        println(s"$startLogoIdx matched $matched")
      }
      startLogoShot.release()
    }
    // (fps * 3 / 2) => groups logo in windows of 1,5 seconds
    val bestMatches = possibleLogoFrames.toVector.groupBy(l => (l.index / (fps * 1.5).toInt ).toInt).map{
      case (idx, logos) =>
        logos.minBy(logo => logo.matches - idx)
    }.toVector.sortBy(_.index)
    
    val replays = collection.mutable.ArrayBuffer.empty[Logo]
    
    for (bestMatch <- bestMatches ) {
      if (!replays.exists(l => (l.matches / fps ).toInt == (bestMatch.index / fps ).toInt)) replays += bestMatch
    }
    
    replays.toVector
  }
  
  
  private def findLogoTemplates(shotIdxs: Seq[Int]) = {
    val possibleLogoFrames = collection.mutable.Set.empty[Logo]
    val sortedShots = shotIdxs.sorted
    for (shotAIdx <- sortedShots) {
      val shotAFile = shotAIdx + "A.png"
      val shotA = Imgcodecs.imread("shot/" + shotAFile, Imgcodecs.IMREAD_UNCHANGED)
      for (shotBIdx <- sortedShots if shotBIdx - shotAIdx >= fps * 2 & shotBIdx - shotAIdx < fps * 90) {
        val shotBFile = shotBIdx + "B.png"
        val shotB = Imgcodecs.imread("shot/" + shotBFile, Imgcodecs.IMREAD_UNCHANGED)
        val res = contourDiff(shotA, shotB)
        if (res > logoThreshold) {
          println(shotAIdx + " matches with " + shotBIdx + " by " + res)
          possibleLogoFrames += Logo(shotAIdx, shotBIdx)
        }
        shotB.release()
      }
      shotA.release()
    }
    possibleLogoFrames.toVector
  }
  
  
  private def getHSVhistogram(img: Mat): HSVhistogram = {
    val hsvImg = new Mat()
    Imgproc.cvtColor(img, hsvImg, Imgproc.COLOR_RGB2HSV)
    
    val vHist = new Mat(256, 1, CvType.CV_8UC1) // todo: check if it's faster with less bins
    val sHist = new Mat(256, 1, CvType.CV_8UC1)
    val hHist = new Mat(256, 1, CvType.CV_8UC1)
    val ranges = new MatOfFloat(0f, 256f)
    val histSize = new MatOfInt(binSize)
    val channel0 = new MatOfInt(0)
    val channel1 = new MatOfInt(1)
    val channel2 = new MatOfInt(2)
    val mask = new Mat()
    
    Imgproc.calcHist(List(hsvImg).asJava, channel0, mask, hHist, histSize, ranges)
    Imgproc.calcHist(List(hsvImg).asJava, channel1, mask, sHist, histSize, ranges)
    Imgproc.calcHist(List(hsvImg).asJava, channel2, mask, vHist, histSize, ranges)
    
    ranges.release()
    histSize.release()
    hsvImg.release()
    channel0.release()
    channel1.release()
    channel2.release()
    mask.release()
    
    HSVhistogram(hHist, sHist, vHist)
  }
  
  private def doROI(
    frame: Mat,
    left: Double = 0.2,
    right: Double = 0.8,
    top: Double = 0.2,
    bottom: Double = 0.8
  ): Mat = {
    val absLeft = (left * videoWidth).toInt
    val absRight = (right * videoWidth).toInt
    val absTop = (top * videoHeight).toInt
    val absBottom = (bottom * videoHeight).toInt
    val res = frame.submat(absTop, absBottom, absLeft, absRight)
    res
  }
  
  private def saveSlidingWindows(
    slidingWindows: Seq[Int],
    tag: String = "",
    windowSize: Int = saveWindowSize,
  ): Unit = {
    slidingWindows.foreach {index =>
      capture.set(Videoio.CAP_PROP_POS_FRAMES, index - windowSize)
      // todo : windowSize * 2 => windowSize
      val edges: Seq[Mat] = (0 until windowSize * 2).map { i =>
        val frame: Mat = new Mat()
        capture.read(frame)
        val resized = new Mat()
        Imgproc.resize(frame, resized, new Size(videoWidth, videoHeight))
        val cropped: Mat = doROI(resized)
        Imgproc.resize(cropped, cropped, new Size(mosaicWidth,mosaicHeight))
        val edged: Mat = doCannyEdgeDetection(cropped)
        Core.copyMakeBorder(edged, edged, mosaicBorderSize,mosaicBorderSize,mosaicBorderSize,mosaicBorderSize, Core.BORDER_CONSTANT, new Scalar(0.0))
        cropped.release()
        frame.release()
      
        edged
      }
      val shiftEdgesFrame = new Mat()
      val normalEdgesFrame = new Mat()
      val shiftEdges: util.List[Mat] = (0 until windowSize * 2).map { i =>
        val shiftEdge = new Mat()
        val edge = edges.indices.map { j =>
          edges( (i + j) % edges.size)
        }.asJava
        Core.hconcat(edge, shiftEdge)
        shiftEdge
      }.asJava
      val normalEdges = (0 until windowSize * 2 ).map { i =>
        val normalEdge = new Mat()
        val edge = edges.asJava
        Core.hconcat(edge, normalEdge)
        normalEdge
      }.asJava
      Core.vconcat(shiftEdges, shiftEdgesFrame)
      Core.vconcat(normalEdges, normalEdgesFrame)
    
      val parameters = new MatOfInt(
        Imgcodecs.IMWRITE_PNG_BILEVEL, 1
      )
      Imgcodecs.imwrite("shot/" + index + "A.png", shiftEdgesFrame, parameters)
      Imgcodecs.imwrite("shot/" + index + "B.png", normalEdgesFrame, parameters)
      shiftEdgesFrame.release()
      normalEdgesFrame.release()
      parameters.release()
      edges.foreach(_.release())
      normalEdges.asScala.foreach(_.release())
      shiftEdges.asScala.foreach(_.release())
    }
  
  }

  
  private def doCannyEdgeDetection(frame: Mat): Mat = {
    val edges = new Mat()
    val dilateWidth = 1
    val dilateKernel = new Mat(new Size(dilateWidth, dilateWidth), CvType.CV_8U, new Scalar(1.0))
    val blurred = new Mat()
    Imgproc.bilateralFilter(frame, blurred, 15, 80,80, Core.BORDER_DEFAULT)
    Imgproc.Canny(blurred, edges, 100, 300, 3)
    Imgproc.dilate(edges, edges, dilateKernel)
    dilateKernel.release()
    blurred.release()
    edges
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
    shotDetector.findShotFrames
  }
}
import java.io.{BufferedWriter, File, FileWriter}
import java.nio.IntBuffer

import scala.collection.JavaConverters._
import org.bytedeco.javacpp.indexer.IntIndexer
import org.bytedeco.javacpp.opencv_videoio.VideoCapture

import org.bytedeco.javacpp.opencv_videoio._
import org.bytedeco.javacpp.opencv_imgcodecs._
import org.bytedeco.javacpp.opencv_imgproc._
import org.bytedeco.javacpp.opencv_core._




class ReplayDetector(capture: VideoCapture) extends VideoModule {

  import OpenCvUtils._
  
  private val fps: Int = math.round(capture.get(CAP_PROP_FPS)).toInt

  // logo detection parameters
  private val mosaicBorderSize = 2
  private val (mosaicWidth, mosaicHeight) = (0.81 * videoWidth + mosaicBorderSize * 2, 0.81 * videoHeight + mosaicBorderSize * 2) // todo : check  "+ mosaicBorderSize * 2"

  
  //private val saveWindowSize = math.min((fps / 6).toInt, 4)
  private val contourDiffDilate: Option[Int] = Some(2)
  private val minLogoSegmentLength = mosaicHeight / 4
  private val logoThreshold = saveWindowSize * 1000
  private val knownLogoThreshold = 2
  
  println(s"Save window size : $saveWindowSize")
  println("Logo threshold : " + logoThreshold)
  println("Min logo segment length : " + minLogoSegmentLength)
  
  // for testing purpose only
  // todo : remove/make private once testing done
  def createPlayList(shotIdx: Seq[Int], replays: Vector[Logo], videoFilename: String) = {
    val playList = new File(videoFilename + "_playlist.m3u")
    playList.createNewFile()
    val bw = new BufferedWriter(new FileWriter(playList))
    bw.write("#EXTM3U\n")
    var i = 0
    val shots = "#EXTVLCOPT:bookmarks=" + shotIdx.sorted.map {idx =>
      val bm = "{name=" + idx + " #" + i + ",bytes=-1,time=" + math.round(idx/fps) + "}"
      i += 1
      bm
    }.mkString(",")
    
    var lastIndex = 0
    val keptLogos = collection.mutable.ArrayBuffer.empty[Logo]
    val sortedReplays = replays.groupBy(_.index).toVector.sortBy(_._1)
    
    for {(logo, matchingLogos) <- sortedReplays if (logo - lastIndex) > fps *2} {
      val bestPair = matchingLogos.minBy(matchingLogo => matchingLogo.matches - logo)
      lastIndex = bestPair.matches
      keptLogos += bestPair
    }
  
    val rp = keptLogos.map {case Logo(beginIdx, endIdx, _, _) =>
      val replayBegin = "{name=1 : " + beginIdx + ",bytes=-1,time=" +  math.round(beginIdx/fps) + "},"
      val replayEnd = "{name=2 : " + endIdx + ",bytes=-1,time=" +  math.round(endIdx/fps) + "}"
      replayBegin + replayEnd
    }.mkString(",")
    
    bw.write(shots + rp + "\n")
    bw.write(System.getProperty("user.dir") + "/" + videoFilename)
    bw.close()
    keptLogos.toVector
  }
  
  def saveBestLogoShots(logoShots: Seq[Logo]): Unit = {
    if (logoShots.nonEmpty) {
      val bestLogo = logoShots.maxBy(_.score)
      saveShots(Vector(bestLogo.index, bestLogo.matches), "known_logo/")
    }
  }
  
  
  // todo : fix, it seems to be O(n²)
  // todo : remove tag
  private def contourDiff(imgA: Mat, imgB: Mat, tag: String = "") = {
    
    val diff = new Mat()
    bitwise_and(imgA, imgB, diff)
    
    val blurredDiff = new Mat()
    diff.copyTo(blurredDiff)
    //Imgproc.GaussianBlur(diff, blurredDiff, new Size(3d,3d), 1,0)
    //Imgproc.threshold(blurredDiff, blurredDiff, 100, 255, Imgproc.THRESH_BINARY)
    contourDiffDilate.foreach(dilateWidth => dilateImg(dilateWidth, blurredDiff))
    //blurAndThresh(diff)
    //erodeImg(2 , diff)
  
    val contours = findContours(blurredDiff)
  
    val score = collection.mutable.Map.empty[(Int, Int), Double] // todo: find better data structure than Map
    val totScore = collection.mutable.Map.empty[Int, Double] // todo: find better data structure than Map
    
    for (contour <- contours.get()) {
      // val contourX:Int = contour(new Range(0,0))(1)
      val contourX = contour.createIndexer[IntIndexer]().get(0,0,1)
      val contourY = contour.createIndexer[IntIndexer]().get(0,0,0)
      val x:Int = (contourX / mosaicWidth).toInt
      val y:Int = (contourY / mosaicHeight).toInt

      val _arcLength = arcLength(contour, true)
      val arcScore = if (_arcLength > minLogoSegmentLength) _arcLength else 0
      val r = score.getOrElse((x,y), 0d) + arcScore
      score.update((x,y), r)
      totScore.update(x, totScore.getOrElse(x, 0d) + r)
    }
    
    val res = if (totScore.nonEmpty) {
      // the contours detected not on the same line as the best line in the mosaic, are most likely noise
      val (bestLine, bestScore) = totScore.toVector.maxBy{case (line, lineScore) => lineScore}
      bestScore
    } else 0d
    
    
    if (res > logoThreshold) {
      //val draw = drawContours(diff, minLogoSegmentLength)
      imwrite("preprocess/" + tag +  "_" + res + "_test.png", blurredDiff)
      //draw.release()
    }
    
    contours.get().foreach(_.release())
    diff.release()
    blurredDiff.release()
    
    res
  }
  

  // finds the logo against our database of known logo (inside the known_logo folder)
  // todo : do not make the known_logo folder hard coded
  def matchKnownLogo(shotIdxs: Seq[Int]) = {
    var videoTag: Option[String] = None
    val logoFolder = new java.io.File("known_logo/")
    val possibleLogoFrames = collection.mutable.Set.empty[Logo]
    val shotsImg = new java.io.File("shot/A/").listFiles().toVector
    for (shotImg <- shotsImg) {
      val shot = imread(shotImg.getCanonicalPath, IMREAD_UNCHANGED)
      val shotIdx = shotImg.getName.split("\\.")(0).toInt
      
      videoTag match {
        case Some(tag) =>
          var found = 0
            for (logoFile <- new java.io.File(logoFolder.getPath + "/" + tag + "/").listFiles() if found < knownLogoThreshold) {
              val logoShot = imread(logoFile.getPath, IMREAD_UNCHANGED)
              val res = contourDiff(shot, logoShot)
              if (res > logoThreshold) {
                found += 1
                println(shotIdx + " matches with " + logoFile.getName + " by " + res)
                if (found >= knownLogoThreshold) possibleLogoFrames += Logo(shotIdx, logoFile.getName.split("\\.")(0).toInt, res.toInt, Some(tag))
              }
              logoShot.release()
            }
          //if (found < knownLogoThreshold) videoTag = None // if we didn't found two match, the tag is reset TODO : remove this reset
        case None =>
          for (logoTag <- logoFolder.listFiles() if videoTag.isEmpty) {
            for (logoFile <- logoTag.listFiles() if videoTag.isEmpty) {
              val logoShot = imread(logoFile.getPath, IMREAD_UNCHANGED)
              val res = contourDiff(shot, logoShot)
              if (res > logoThreshold) {
                println("************** VIDEO TAG : " + logoTag.getName)
                videoTag = Some(logoTag.getName)
                println(shotIdx + " matches with NONE TAG " + logoFile.getPath + " by " + res)
                possibleLogoFrames += Logo(shotIdx, logoFile.getName.split("\\.")(0).toInt, res.toInt, Some(logoTag.getName))
              }
              logoShot.release()
            }
          }
      }
      shot.release()
    }
    possibleLogoFrames.toVector
  }
  
  
  private def filterlogo(logoTemplates: Seq[Logo]) = {
    val possibleLogoFrames = collection.mutable.ArrayBuffer.empty[Logo]
    val startLogoIdxPool = logoTemplates.groupBy(_.index)
    val endLogoIdxPool = logoTemplates.groupBy(_.matches)
    for ((startLogoIdx, matchingFrames) <- startLogoIdxPool) {
      val startLogoFile = startLogoIdx + "A.png"
      val startLogoShot = imread("shot/" + startLogoFile, IMREAD_UNCHANGED)
      var matched = 0
      for ((endLogoIdx, endLogo) <- endLogoIdxPool)
        if (math.abs(endLogoIdx - startLogoIdx) > fps * 2 & matched <= 2)
        {
          val endLogoFile = endLogoIdx + "B.png"
          val endLogoShot = imread("shot/" + endLogoFile, IMREAD_UNCHANGED)
          val res = contourDiff(startLogoShot, endLogoShot)
          if (res > logoThreshold * 2 ) {
            matched += 1
          }
          endLogoShot.release()
        }
      if (matched > 2) {
        possibleLogoFrames += matchingFrames.minBy(logo => logo.matches - startLogoIdx)
        println(s"$startLogoIdx matched $matched")
      }
      startLogoShot.release()
    }
    possibleLogoFrames.toVector
  }
  
  
  def findReplay(foundLogos: Vector[Logo]) = {
    foundLogos.groupBy(l => l.index).map{
      case (idx, logos) =>
        logos.minBy(logo => logo.matches - idx)
    }.toVector.sortBy(_.index)
  }
  
  
  def findLogo(shotIdxs: Seq[Int]) = {
    val possibleLogoFrames = collection.mutable.Set.empty[Logo]
    val sortedShots = shotIdxs.sorted
    for (shotAIdx <- sortedShots) {
      val shotAFile = shotAIdx + ".png"
      val shotA = imread("shot/A/" + shotAFile, IMREAD_UNCHANGED) // todo : dont hard code shot/A/
      for (shotBIdx <- sortedShots if shotBIdx - shotAIdx >= fps * 2 & shotBIdx - shotAIdx < fps * 90) {
        val shotBFile = shotBIdx + ".png"
        val shotB = imread("shot/B/" + shotBFile, IMREAD_UNCHANGED) // todo : dont hard code shot/A/
        val res = contourDiff(shotA, shotB, shotAIdx + "_" + shotBIdx)
        if (res > logoThreshold) {
          println(shotAIdx + " matches with " + shotBIdx + " by " + res)
          possibleLogoFrames += Logo(shotAIdx, shotBIdx, res.toInt)
        }
        shotB.release()
      }
      shotA.release()
    }
    possibleLogoFrames.toVector
  }
  
  
  private def makeBackgroundFromIndex(idx: Int, nbOfBgFrame: Int): Vector[Mat] = {
    capture.set(CAP_PROP_POS_FRAMES, idx)
    (0 until nbOfBgFrame).toVector.map{ i =>
      // read and resize
      val bgFrame = new Mat()
      capture.read(bgFrame)
      resize(bgFrame, bgFrame, new Size(videoWidth,videoHeight))
      val centered = cropImage(bgFrame)
      val contoursFrame = makeCountoursFrame(centered, minLogoSegmentLength)
      // release all
      bgFrame.release()
      centered.release()
      contoursFrame
    }
  }
  
  
  /*
  * Writes the shots (indexed by their frame position) on disk, to be analyzed by the replay detection later.
  * */
  def saveShots(shotIdxs: Vector[Int], shotFolder: String = "shot/", frameFolder: String="frame/"): Unit = {

    // Creates an image in the shape of a mosaic, of all the frame in a window defined by windowSize around the shot index.
    // The background frames will be substracted in every frame in the mosaic
    // backgroundFrame must be of dimension mosaicWidth * mosaicHeight
    // todo : check the release here
    def writeShotMosaic(shotIdx: Int, backgroundFrames: Vector[Mat] = Vector.empty[Mat]) = {
      // first, we save the frames in the window before the shot transition

        val framePosition = shotIdx - saveWindowSize // half of the frame in the window are behind the current index
        capture.set(CAP_PROP_POS_FRAMES, framePosition)
        // all the border detected in the frames
        val edges: Seq[Mat] = (0 until saveWindowSize).map { i =>
          val frame: Mat = new Mat()
          capture.read(frame)
          val resized = new Mat()
          resize(frame, resized, new Size(videoWidth, videoHeight))
          val cropped: Mat = cropImage(resized)

          val contours = makeCountoursFrame(cropped, minLogoSegmentLength)
          val edgedWithRemovedBackground = new Mat()
          contours.copyTo(edgedWithRemovedBackground)
          backgroundFrames.foreach{bg =>
            subtract(edgedWithRemovedBackground, bg, edgedWithRemovedBackground)
          }

          copyMakeBorder(edgedWithRemovedBackground, edgedWithRemovedBackground, mosaicBorderSize,mosaicBorderSize,mosaicBorderSize,mosaicBorderSize, BORDER_CONSTANT, new Scalar(0.0))

          cropped.release()
          frame.release()
          contours.release()
          resized.release()

          edgedWithRemovedBackground
        }
        val shiftEdgesFrame = new Mat()
        val normalEdgesFrame = new Mat()
        val shiftEdges = (0 until saveWindowSize).map { i =>
          val shiftEdge = new Mat()
          val edge = edges.indices.map { j =>
            edges( (i + j) % edges.size)
          }
          hconcat(new MatVector(edge:_*), shiftEdge)
          shiftEdge
        }
        val normalEdges = (0 until saveWindowSize).map { i =>
          val normalEdge = new Mat()
          hconcat(new MatVector(edges:_*), normalEdge)
          normalEdge
        }
        vconcat(new MatVector(shiftEdges:_*), shiftEdgesFrame)
        vconcat(new MatVector(normalEdges:_*), normalEdgesFrame)

        // TODO : re enable this
        /*
        val parameters = new IntBuffer(
          IMWRITE_PNG_BILEVEL, 1
        )
        */
        imwrite(shotFolder + "A/" + framePosition + ".png", shiftEdgesFrame/*, parameters*/)
        imwrite(shotFolder + "B/" + framePosition + ".png", normalEdgesFrame/*, parameters*/) // todo: remove this in 'prod'
        shiftEdgesFrame.release()
        normalEdgesFrame.release()
        //parameters.release()
        edges.foreach(_.release())
        normalEdges.foreach(_.release())
        shiftEdges.foreach(_.release())

      backgroundFrames.foreach(_.release())
    }

    var previousShotIdx = 0
    val backgroundSize = 1

    for {(_shotIdx, i) <- shotIdxs.zipWithIndex} {
      for (shotIdx <- (0 to numberOfWindow).map(_ + _shotIdx)) {
        val nextBackgroundFrames = shotIdxs.lift(i + 1).toVector.flatMap { nextShotIdx => // We remove from the current frame the frames in the middle of the current shot and of the next shot
          // We need to make sure that we don't take frames outside of the next shot (hence the min)
          // or frame that could be logo frame (hence the + fps, because a logo roughly last for 1 second)
          //val nextFrameBackgroundIdx = ((shotIdx + nextShotIdx) / 2).toInt
          val nextFrameBackgroundIdx = shotIdx + (fps * 2 / 3).toInt
          val nextBackgroundFrames = makeBackgroundFromIndex(nextFrameBackgroundIdx, backgroundSize) //if (shotIdx + fps > nextFrameBackgroundIdx) Vector.empty[Mat] // make sure that we don't go the next shot
          //else makeBackgroundFromIndex(nextFrameBackgroundIdx, backgroundSize)
          nextBackgroundFrames
        }

        // We remove from the current frame the frames in the middle of the current shot and of the previous shot
        // We need to make sure that we don't take frames outside of the previous shot (hence the max)
        // or frame that could be logo frame (hence the - fps, because a logo roughly last for 1 second)
        //val previousBackgroundFrameIndex = ((shotIdx - fps + previousShotIdx) / 2).toInt
        val previousBackgroundFrameIndex = shotIdx - (fps * 2 / 3).toInt - backgroundSize
        val previousBackgroundFrames = makeBackgroundFromIndex(previousBackgroundFrameIndex, backgroundSize)
        if (previousBackgroundFrameIndex + fps > shotIdx) Vector.empty[Mat] else makeBackgroundFromIndex(previousBackgroundFrameIndex, backgroundSize)

        writeShotMosaic(shotIdx, nextBackgroundFrames ++ previousBackgroundFrames) // todo : reenable nextBGframes
        previousShotIdx = shotIdx

        //nextBackgroundFrames.foreach(_.release())
        previousBackgroundFrames.foreach(_.release())
      }
    }
  }
}
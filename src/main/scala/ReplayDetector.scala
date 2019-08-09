import java.io.{BufferedWriter, File, FileWriter}
import java.nio.IntBuffer

import scala.collection.JavaConverters._
import org.opencv.videoio.Videoio._
import org.opencv.imgcodecs.Imgcodecs._
import org.opencv.imgproc.Imgproc._
import org.opencv.core.Core._
import org.opencv.core.CvType._
import org.opencv.core.{Mat, MatOfPoint, MatOfPoint2f, Scalar, Size}
import org.opencv.videoio.VideoCapture




class ReplayDetector(capture: VideoCapture) extends Configuration {

  import OpenCvUtils._

  private val fps: Int = math.round(capture.get(CAP_PROP_FPS)).toInt

  // logo detection parameters
  private val mosaicBorderSize: Int = 2
  private val (mosaicWidth, mosaicHeight) = ((0.81 * videoWidth + mosaicBorderSize * 2).toInt, (0.81 * videoHeight + mosaicBorderSize * 2).toInt)


  //private val saveWindowSize = math.min((fps / 6).toInt, 4)
  private val contourDiffDilate: Option[Int] = Some(2)
  private val minLogoSegmentLength = mosaicHeight / 4
  private val logoThreshold = mosaicSize * 2000 // !!! the logo threshold is dependant of the size of the image (the countours are bigger in a 200*200 img than in a 100*100 img)
  private val knownLogoThreshold = 2 // the number of known logo that must match

  println(s"Save window size : $mosaicSize")
  println("Logo threshold : " + logoThreshold)
  println("Min logo segment length : " + minLogoSegmentLength)

  // for testing purpose only
  // todo : remove/make private once testing done
  def createPlayList(
    shotIdx: Seq[Int],
    videoFilename: String,
    replays: Vector[Logo] = Vector(),
    logos: Vector[Logo] = Vector()
  ): Unit = {
    val playList = new File(videoFilename + "_playlist.m3u")
    playList.createNewFile()
    val bw = new BufferedWriter(new FileWriter(playList))
    bw.write("#EXTM3U\n")
    var i = 0

    val shotRows = "#EXTVLCOPT:bookmarks=" + shotIdx.sorted.map {idx =>
      val bm = "{name=" + idx + " #" + i + ",bytes=-1,time=" + math.round(idx/fps) + "}"
      i += 1
      bm
    }.mkString(",")

    val replayRows = replays.map {case Logo(beginIdx, endIdx, _, _) =>
      val replayBegin = "{name=1 : " + beginIdx + ",bytes=-1,time=" +  math.round(beginIdx/fps) + "},"
      val replayEnd = "{name=2 : " + endIdx + ",bytes=-1,time=" +  math.round(endIdx/fps) + "}"
      replayBegin + replayEnd
    }.mkString(",")

    val logoRows = logos.sortBy(_.index).map(_.index).distinct.map{ idx =>
      "{name=Logo : " + idx + ",bytes=-1,time=" +  math.round(idx/fps) + "},"
    }.mkString(",")

    bw.write(shotRows + replayRows + logoRows + "\n")
    bw.write(System.getProperty("user.dir") + "/" + videoFilename)
    bw.close()
  }

  // todo : fix, it seems to be O(n²)
  // todo : remove tag
  /**
    * Returns a score corresponding to how much the contours are similar between to images.
    * @param imgA The first image
    * @param imgB The second image
    * @param tag Useful for debugging purpose, if the score is above a threshold, an image is created with the contours
    *            that matched between the two images in the "preprocess" folder (might rename it to something more
    *            meaningful), the tag is the name that this image must have (typically, [imgA_idx]_[imgB_idx].png)
    * @return
    */
  private def contourDiff(imgA: Mat, imgB: Mat, tag: String = "") = {

    val diff = new Mat()
    bitwise_and(imgA, imgB, diff) // the bitwise & is what finds the pixels that are exactly similar in the two images

    val blurredDiff = new Mat()
    diff.copyTo(blurredDiff) // actually we don't do any blurring
    //Imgproc.GaussianBlur(diff, blurredDiff, new Size(3d,3d), 1,0)
    //Imgproc.threshold(blurredDiff, blurredDiff, 100, 255, Imgproc.THRESH_BINARY)
    contourDiffDilate.foreach(dilateWidth => dilateImg(dilateWidth, blurredDiff))

    // penalize img with too much white pixels
    // TODO : investigate by changing the whitePenalizationRatio value
    val whiteA = countNonZero(imgA).toDouble / (imgA.width() * imgA.height())
    val whiteB = countNonZero(imgB).toDouble / (imgB.width() * imgB.height())
    val whitePenalizationRatio = 5

    // Detects the contours in the image resulting from the binary & between imgA and imgB
    val contours = findContours(blurredDiff)

    // The scores of all the frame in the mosaic, indexed by their coordinates (x,y) in the mosaic
    // todo: find better data structure than Map
    val framesScore = collection.mutable.Map.empty[(Int, Int), Double]
    // The scores for each rows in the mosaic, indexed by a row number x
    // A row in the mosaic is like a continuous shot, that's why the score of each row is interesting
    val rowsScore = collection.mutable.Map.empty[Int, Double] // todo: find better data structure than Map
    for (idx <- 0 until contours.size().toInt) {
      // contourX and contourY are where in the mosaic frame the contour is (in pixels)
      val contourX = contours.get(idx).get(1,0)(0)
      val contourY = contours.get(idx).get(0,0)(0)
      // a mosaic is a matrix of dim U * V; an area (a square) in the mosaic is represented by two integers x and y
      // so basically (x,y) are coordinates that allows us to locate a frame in the mosaic
      // (because a mosaic is a matrix of frames)
      val x:Int = (contourX / mosaicWidth).toInt
      val y:Int = (contourY / mosaicHeight).toInt

      val contour = new MatOfPoint2f()
      contours.get(idx).convertTo(contour, CV_32F)
      val _arcLength = arcLength(contour, true)
      val arcScore =
        if (_arcLength > minLogoSegmentLength)
          _arcLength * (1 - whiteA * whitePenalizationRatio) * (1 - whiteB * whitePenalizationRatio)
        else 0
      // there can be multiple contours in a frame at position x y in the mosaic, so we retrieve the previous score at position x y
      // and add the score for the countour
      val scoreForFrame = framesScore.getOrElse((x,y), 0d) + arcScore
      // store the the score for a frame in framesScore (x,y)
      framesScore.update((x,y), scoreForFrame)
      rowsScore.update(x, rowsScore.getOrElse(x, 0d) + scoreForFrame)
    }

    val res = if (rowsScore.nonEmpty) {
      // the contours detected not on the same line as the best line in the mosaic are most likely noise
      val (bestLine, bestScore) = rowsScore.toVector.maxBy{case (row, rowScore) => rowScore}
      bestScore
    } else 0d


    if (res > logoThreshold && debugMode) {
      //val draw = drawContours(diff, minLogoSegmentLength)
      imwrite("preprocess/" + tag +  "_" + res + ".png", blurredDiff)
      //draw.release()
    }

    for (idx <- 0 until contours.size().toInt) {
      contours.get(idx).release()
    }

    diff.release()
    blurredDiff.release()

    res
  }


  // finds the logo against our database of known logo (inside the known_logo folder)
  // todo : do not make the known_logo folder hard coded
  def matchKnownLogo(shotIdxs: Seq[Int], tag: String) = {
    val logoFolder = new java.io.File(knownLogoFolderName)
    val possibleLogoFrames = collection.mutable.Set.empty[Logo]
    val shotsPath = shiftedMosaicFolderName
    for (shotIdx <- shotIdxs) {
      val shot = imread(shotsPath + shotIdx + ".png", IMREAD_UNCHANGED)
      var found = 0
      for (logoFile <- new java.io.File(logoFolder.getPath + "/" + tag + "/").listFiles() if found < knownLogoThreshold) {
        val logoShot = imread(logoFile.getPath, IMREAD_UNCHANGED)
        val res = contourDiff(shot, logoShot)
        if (res > logoThreshold) {
          found += 1
          println(shotIdx + " matches with " + logoFile.getName + " by " + res)
          if (found >= knownLogoThreshold) possibleLogoFrames += Logo(shotIdx, shotIdx, res.toInt, Some(tag))
        }
        logoShot.release()
      }

      shot.release()
    }
    possibleLogoFrames.toVector
  }

  def findReplay(foundLogos: Vector[Logo]): Vector[Logo] = {
    foundLogos.groupBy(l => l.index).map{
      case (idx, logos) =>
        logos.minBy(logo => logo.matches - idx)
    }.toVector.sortBy(_.index)
  }

  /**
    * FIXME: the execution time doesn't seeam linear, need to investigate this
    * Compares the mosaics of shots to find matching contours.
    * A score relative to the contours is given (), and if this score is above a threshold, then considers the two frames
    * like logos that match with each other.
    * @param shotIdxs The indexes (frame position in the video) of the shots
    * @return The detected logos
    */
  def findMachingShots(shotIdxs: Seq[Int]): Vector[Logo] = {
    val possibleLogoFrames = collection.mutable.Set.empty[Logo]
    val sortedShots = shotIdxs.sorted
    for (shotAIdx <- sortedShots) {
      val shiftMosaicFilename = shotAIdx + ".png"
      val shiftMosaic = imread(shiftedMosaicFolderName + shiftMosaicFilename, IMREAD_UNCHANGED) // todo : dont hard code shot/A/
      for (shotBIdx <- sortedShots if shotBIdx - shotAIdx >= fps * 2 & shotBIdx - shotAIdx < fps * 90) {
        val normalMosaicFilename = shotBIdx + ".png"
        val normalMosaic = imread(normalMosaicFolderName + normalMosaicFilename, IMREAD_UNCHANGED) // todo : dont hard code shot/A/
        val res = contourDiff(shiftMosaic, normalMosaic, shotAIdx + "_" + shotBIdx) // we get the score of the matching contours between the two frames
        if (res > logoThreshold) {
          println(shotAIdx + " matches with " + shotBIdx + " by " + res)
          possibleLogoFrames += Logo(shotAIdx, shotBIdx, res.toInt) += Logo(shotBIdx, shotAIdx, res.toInt) // if they match, we add both the begin and the end logo
        }
        normalMosaic.release()
      }
      shiftMosaic.release()
    }
    val doubleCheck = false
    // because each potential logo is doubled (see the comment above, we add both the logo and its matching conterpart
    // to the list of potential logo), requiredNumberOfMatchingLogosForEachLogo is also doubled
    val requiredNumberOfMatchingLogosForEachLogo = 4 * 2
    // When we have found all the potential logos, we make a double check.
    // This double check consists of keeping only the logos that matches with at least N others logos.
    // This prevents false positive but may be bad if the video is too short.
    // Adds quite some execution time to the function, (see the to-do above).
    if (doubleCheck) {
      val filteredLogosIdx = possibleLogoFrames.map(_.index)
      for (idx <- possibleLogoFrames.map(_.index).toVector) {
        val otherLogoIndexes = (filteredLogosIdx - idx).toVector
        // we ensure that at least another N match
        val atLeastNOtherMatch = existsN(otherLogoIndexes, {otherIdx: Int =>
          val shiftMosaic = imread(shiftedMosaicFolderName + idx + ".png", IMREAD_UNCHANGED)
          val normalMosaic = imread(normalMosaicFolderName + otherIdx + ".png", IMREAD_UNCHANGED)
          val score = contourDiff(shiftMosaic, normalMosaic, idx + "_" + otherIdx)
          shiftMosaic.release()
          normalMosaic.release()
          score > logoThreshold
        }, requiredNumberOfMatchingLogosForEachLogo)
        if (! atLeastNOtherMatch) filteredLogosIdx -= idx
      }
      possibleLogoFrames.filter(logo => filteredLogosIdx.contains(logo.index)).toVector
    }
    else possibleLogoFrames.toVector
  }

  private def existsN[A](seq: Vector[A], pred: A => Boolean, n: Int): Boolean = {
    if (n == 0) true
    else {
      seq match {
        case Vector() => false
        case head +: tail =>
          existsN(tail, pred, if (pred(head)) n - 1 else n)
      }
    }
  }


  /**
    * A background frame is a countours image. It will be used to filter the still pixels in the shots.
    * Typically, the background frames will be those at the middle at the shot to be sure we don't remove logo
    * information.
    * @param idx The starting frame that will serve as background
    * @param nbOfBgFrame The number of background frames (if more than one, we take the frames after the start frame)
    * @return Background frames
    */
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
  * The shots are saved as mosaic images. See the comment in the trait Configuration.
  * */
  def saveShots(
    shotIdxs: Vector[Int],
    shotFolder: String = mosaicParentFolderName,
    frameFolder: String=frameFolderName
  ): Unit = {
    var previousShotIdx = 0
    val backgroundSize = 1

    for {(shotIdx, i) <- shotIdxs.zipWithIndex} {

      val nextBackgroundFrames = {
        // We remove from the current frame the frames in the middle of the current shot and of the next shot
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
      //val s = if (previousBackgroundFrameIndex + fps > shotIdx) Vector.empty[Mat] else makeBackgroundFromIndex(previousBackgroundFrameIndex, backgroundSize)

      writeShotMosaic(shotIdx, nextBackgroundFrames ++ previousBackgroundFrames, shotFolder)
      previousShotIdx = shotIdx

      //nextBackgroundFrames.foreach(_.release())
      previousBackgroundFrames.foreach(_.release())
    }
  }

  // Creates an image in the shape of a mosaic, of all the frame in a window defined by windowSize around the shot index.
  // The background frames will be substracted in every frame in the mosaic
  // backgroundFrame must be of dimension mosaicWidth * mosaicHeight
  // todo : check the release here
  private def writeShotMosaic(shotIdx: Int, backgroundFrames: Vector[Mat] = Vector.empty[Mat], shotFolder: String) = {
    // We want to save frames surrounding the shot transition (half of them in shot S_t and half of them in shot S_t+1 ideally)
    // The frames before the shot transition are those before the current index and the frames after the shot transition
    // are those after the current shot index (shotIdx should ideally be the exact moment between two shots).
    val framePosition = shotIdx - (mosaicSize / 2) // TODO : check if framePosition = shotIdx - mosaicSize / 2 isn't better
    capture.set(CAP_PROP_POS_FRAMES, framePosition)
    // all the border detected in the frames
    val contours: Seq[Mat] = (0 until mosaicSize).map { _ =>
      val frame: Mat = new Mat()
      capture.read(frame)
      val resized = new Mat()
      resize(frame, resized, new Size(videoWidth, videoHeight))
      // we crop so we don't have static info such as a scoreboard on the image (because they're usually on the edge of the screen)
      val cropped: Mat = cropImage(resized)
      val blurred = new Mat()
      // detects the contour in the frame
      val contoursWithBackground = makeCountoursFrame(cropped, minLogoSegmentLength)
      // we remove the background to remove the still pixels in the frame (a logo is supposed to move, so still
      // pixels are all noise). Still pixels are the pixels that are the same between two frames in the same shots.
      val contoursWithBackgroundRemoved = new Mat()
      contoursWithBackground.copyTo(contoursWithBackgroundRemoved)
      backgroundFrames.foreach{bg =>
        subtract(contoursWithBackgroundRemoved, bg, contoursWithBackgroundRemoved)
      }

      // add a border to the frame in the shot
      copyMakeBorder(contoursWithBackgroundRemoved, contoursWithBackgroundRemoved, mosaicBorderSize,mosaicBorderSize,mosaicBorderSize,mosaicBorderSize, BORDER_CONSTANT, new Scalar(0.0))

      cropped.release()
      frame.release()
      contoursWithBackground.release()
      resized.release()

      contoursWithBackgroundRemoved
    }

    val shiftedContours = (0 until mosaicSize).map { i =>
      val shiftEdge = new Mat()
      val edge = contours.indices.map { j =>
        contours( (i + j) % contours.size)
      }
      hconcat(edge.asJava, shiftEdge)
      shiftEdge
    }

    // non shifted contours
    val normalContours = (0 until mosaicSize).map { i =>
      val normalEdge = new Mat()
      hconcat(contours.asJava, normalEdge)
      normalEdge
    }

    val shiftedContoursMosaic = new Mat()
    val contoursMosaic = new Mat()
    vconcat(shiftedContours.asJava, shiftedContoursMosaic)
    vconcat(normalContours.asJava, contoursMosaic)

    // TODO : re enable this
    /*
    val parameters = new IntBuffer(
      IMWRITE_PNG_BILEVEL, 1
    )
    */
    imwrite(shiftedMosaicFolderName + shotIdx + ".png", shiftedContoursMosaic/*, parameters*/) // the shifted mosaic are stored in the shot/A
    imwrite(normalMosaicFolderName + shotIdx + ".png", contoursMosaic/*, parameters*/) // the non shifted mosaic are stored in the shot/B
    shiftedContoursMosaic.release()
    contoursMosaic.release()
    //parameters.release()
    contours.foreach(_.release())
    normalContours.foreach(_.release())
    shiftedContours.foreach(_.release())

    backgroundFrames.foreach(_.release())
  }
}
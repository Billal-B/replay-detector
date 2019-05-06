/*
import org.opencv.core.{Core, Scalar, _}
import org.opencv.features2d.{BOWImgDescriptorExtractor, FastFeatureDetector, ORB}
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc
import org.opencv.highgui.HighGui
import org.opencv.videoio.{VideoCapture, Videoio}

import scala.collection.JavaConverters._


class LogoDetector(
  videoPath: String,
  frameToAnalyse: Int = Int.MaxValue
) {
  System.loadLibrary(Core.NATIVE_LIBRARY_NAME)
  private val (width, height) = (224,224)
  private val capture = new VideoCapture(videoPath)
  private val skipFrames = 0
  private val fps: Double = capture.get(Videoio.CAP_PROP_FPS)
  private val windowSize: Int = (fps * 5 / 6).toInt / (skipFrames + 1)
  private val frameNumber: Int = math.min(capture.get(Videoio.CAP_PROP_FRAME_COUNT).toInt, frameToAnalyse) - windowSize - 1
  private val luminanceThreshold: Int = (fps * 5 / 6).toInt * 2000
  
  private val binSize = 256
  private val threshold1 = 0.080
  private val threshold2 = 15d
  
  private val orb = org.opencv.features2d.ORB.create(500)
  
  println(s"Luminance threshold is $luminanceThreshold")
  
  private def grayHistogram(mat: Mat): Mat = {
    val hist = new Mat(256, 1, CvType.CV_8UC1)
    val channels = new MatOfInt(0)
    val ranges = new MatOfFloat(0f, 256f)
    val histSize = new MatOfInt(binSize)
    
    Imgproc.calcHist(List(mat).asJava, channels, new Mat(), hist, histSize, ranges)
    hist
  }
  
  private def luminanceDiff(mat: Mat): Double = {
    val hist = grayHistogram(mat)
    
    val (lumDiff, _) = (1 until binSize).foldLeft((0d, hist.get(0,0).sum)){case ((total, prevHistValue), bin) =>
      val histValue = hist.get(bin, 0).sum
      val num = math.pow(histValue - prevHistValue, 2)
      val denum = math.max(histValue, prevHistValue)
      if (denum != 0) (total + (num / denum), histValue)
      else (total, histValue)
    }
    lumDiff
  }
  
  private def luminanceDiff(prevMat: Mat, mat: Mat): Double = {
    val hist = grayHistogram(mat)
    val prevHist = grayHistogram(prevMat)
    
    (0 until binSize).foldLeft(0d){(total, bin) =>
      val histValue = hist.get(bin, 0).sum
      val prevHistValue = prevHist.get(bin, 0).sum
      val num = math.pow(histValue - prevHistValue, 2)
      val denum = math.max(histValue, prevHistValue)
      if (denum != 0) total + (num / denum)
      else total
    }
  }
  
  private def accumulatedDiff(frames: List[Mat]): Double = {
    frames.sliding(2).foldLeft(0d){case (total, f1 :: f2 :: _) =>
      total + luminanceDiff(f1, f2)
    }
  }
  
  private def meanLuminance(frame: Frame): Double = {
    val hist = frame.hist.getOrElse(grayHistogram(frame.matrix))
    
    val num = (0 until binSize).foldLeft(0.0) {(tot, bin) =>
      tot + hist.get(bin, 0).sum * bin
    }
    val denum = (0 until binSize).foldLeft(0.0) {(tot, bin) =>
      tot + hist.get(bin, 0).sum
    }
    
    num / denum
  }
  
  // find maximum length replays
  private def findReplay(logos: Seq[Frame]): Seq[Replay] = {
    val maxDist = 30 * fps
    val minDist = 2 * fps
    val (_, replays) = logos.foldLeft(0, Seq.empty[Replay]) {
      case ((lastReplayIndex, allReplay), logoA) =>
        if (logoA.index <= lastReplayIndex) (lastReplayIndex, allReplay)
        else {
          val closeLogos = logos.filter(logoB =>
            logoB.index - logoA.index > 0 &&
              logoB.index - logoA.index < maxDist &&
              logoB.index - logoA.index > minDist
          )
          val matchingLogo = if (closeLogos.nonEmpty) {
            Some(Replay(logoA.index, closeLogos.maxBy(_.index).index))
          } else None
          val replays = allReplay ++ matchingLogo.toSeq
          val lastLogoIndex = matchingLogo.map(_.end).getOrElse(lastReplayIndex)
          (lastLogoIndex, replays)
        }
    }
    replays
  }
  
  private def findReplaysMin(logos: Seq[Frame]): Seq[Replay] = {
    val maxDist = 50 * fps
    val minDist = 2 * fps
    val (_, replays) = logos.foldLeft(0, Seq.empty[Replay]) {
      case ((lastReplayIndex, allReplay), logoA) =>
        if (logoA.index <= lastReplayIndex) (lastReplayIndex, allReplay)
        else {
          val matchingLogoIndex = logos.find(logoB =>
            logoB.index - logoA.index > 0 &&
              logoB.index - logoA.index < maxDist &&
              logoB.index - logoA.index > minDist
          ).map(_.index)
  
          val replay = matchingLogoIndex.map(idx => Replay(logoA.index, idx))
          val replays = allReplay ++ replay
          (matchingLogoIndex.getOrElse(lastReplayIndex), replays)
        }
    }
    replays
  }
  
  private def groupFrames(frames: Seq[Frame]): Seq[Seq[Frame]] = {
    val (groupedFrames, _, _) = frames.foldLeft(Seq.empty[Seq[Frame]], Seq.empty[Frame], 0){case ((allLogoSeq, logoSeq, prevIndex), logo) =>
      if (logo.index - prevIndex <= windowSize ) (allLogoSeq, logoSeq :+ logo, logo.index)
      else (allLogoSeq :+ logoSeq, Seq(logo), logo.index)
    }
    groupedFrames
  }
  
  private def findPotentialLogo: Seq[Frame] = {
    println(s"Lum : $luminanceThreshold\t")
    
    println(s"Analysing $frameNumber frames")
    
    // read the first frames
    val firstFrames = 0.until(windowSize).map {index =>
      (0 until skipFrames).foreach(_ => capture.read(new Mat()))
      val mat = new Mat()
      capture.read(mat)
      Imgproc.resize(mat, mat, new Size(width, height))
      //Imgproc.cvtColor(mat, mat, Imgproc.COLOR_RGB2GRAY)
      Frame(index, mat)
    }
    
    val (_, logos) = windowSize.to(frameNumber / (skipFrames + 1) - 50)
      .foldLeft(firstFrames.map(_.matrix).toList, Seq[Frame]()) {
        case ((prevWindow, allPotentialLogos), prevIndex) =>
          
          val currentFrameMatrix = new Mat()
          (0 until skipFrames).foreach(_ => capture.read(new Mat()))
  
          capture.read(currentFrameMatrix)
          
          if (new java.util.Random().nextInt(500) > 490) saveLogos(Seq(Frame(prevIndex, currentFrameMatrix)), "non_logos")
  
          val index = prevIndex * (skipFrames + 1)

          if (index % 5000 == 0) println(s"Processed $index frames")
  
          Imgproc.resize(currentFrameMatrix, currentFrameMatrix, new Size(width, height))
          //Imgproc.cvtColor(currentFrameMatrix, currentFrameMatrix, Imgproc.COLOR_RGB2GRAY)
          
          val currentWindow = prevWindow.tail :+ currentFrameMatrix
          val currentFrame = currentWindow.head
          val accDiff = accumulatedDiff(currentWindow)
          if (accDiff > luminanceThreshold) {
            val hist = grayHistogram(currentFrame)
            val potentialLogo = Frame(index - windowSize, currentFrame, Some(hist), accDiff)

            println(s"Potential logo at ${potentialLogo.index} " +
              s"\tTime :${(potentialLogo.index / 25 / 60).toInt}m ${(potentialLogo.index / 25 % 60).toInt}s " +
              s"\tLum : $accDiff")
            (currentWindow, allPotentialLogos :+ potentialLogo)
          } else (currentWindow, allPotentialLogos)
      }
    
    capture.release()
    logos
  }
  
  private def clusterizeLogos(logos: Seq[Frame]): (Seq[Frame], Double) = {
    val labels = new Mat()
    val centers = new Mat()
    val criteria = new TermCriteria(TermCriteria.EPS + TermCriteria.MAX_ITER, 100, 0.1)
    val k = 2
    val logosLuminance = logos.map(meanLuminance)
    val sample = new Mat(logos.size, 1, CvType.CV_32F)
    
    logosLuminance.zipWithIndex.map{ case (lum, idx) =>
      sample.put(idx, 0, lum)
    }
    println("Sorting " + logos.size + " logos")
    Core.kmeans(sample, k, labels, criteria, 3, Core.KMEANS_PP_CENTERS, centers)
    val group0center = centers.get(0,0).head
    val group1center = centers.get(1,0).head
    val logoCenter = if (group0center > group1center) 0 else 1
    val indexedLabel = logosLuminance.indices.map {idx =>
      val predLabel = labels.get(idx, 0).head.toInt
      (idx, predLabel)
    }
    
    val clusterCenter = math.max(group0center, group1center)
    val selectedLogos = (logos zip indexedLabel).filter{case (_, (_, label)) => label == logoCenter}.map(_._1)
    (selectedLogos, clusterCenter)
  }
  
  
  private def frameDistance(m: Frame, n:Frame): Double = {
    val mHist = m.hist.getOrElse(grayHistogram(m.matrix))
    val nHist = n.hist.getOrElse(grayHistogram(n.matrix))
    
    val mHistTotal = 0.until(binSize).map(i => mHist.get(i, 0).sum).sum
    val nHistTotal = 0.until(binSize).map(i => nHist.get(i, 0).sum).sum
    
    val powDist = 0.until(binSize).map { i =>
      val histDiff = (mHist.get(i, 0).sum / mHistTotal) - (nHist.get(i, 0).sum /nHistTotal)
      math.pow(histDiff, 2)
    }.sum
    
    math.sqrt(powDist)
  }
  
  private def saveLogos(logos: Seq[Frame], tag: String): Unit = {
    logos.foreach{logo =>
      Imgcodecs.imwrite(s"$tag/${logo.index}.jpg", logo.matrix)
    }
  }
  
  private def findLogoTemplate(logoFrames: Seq[Frame]): (Frame, Double) = {
    println("searching logo template")
    val res = logoFrames.map { logoA =>
      val logoAhist = logoA.hist.getOrElse(grayHistogram(logoA.matrix))
      val logoAhistTot = 0.until(binSize).map(i => logoAhist.get(i, 0).sum).sum
      val logoDistances = logoFrames.foldLeft(0d) { (totalDist, logoB) =>
        val logoBhist = logoB.hist.getOrElse(grayHistogram(logoB.matrix))
        val logoBhistTot = 0.until(binSize).map(i => logoBhist.get(i, 0).sum).sum
        val distToLogoB = 0.until(binSize).foldLeft(0d){(distToLogo,i) =>
          val histDiff = (logoAhist.get(i, 0).sum /logoAhistTot) - (logoBhist.get(i, 0).sum /logoBhistTot)
          distToLogo + math.pow(histDiff, 2)
        }
        totalDist + distToLogoB
      }
      (logoA, logoDistances)
    }.minBy{case(_, distance) => distance}
    println("found logo template")
    res
  }
  
  private def matchLogosToTemplate(logoFrames: Seq[Frame], logoTemplate: Frame, clusterCenter: Double): Seq[Frame] = {
    println("cluster center: " + clusterCenter)
    logoFrames.filter {logoFrame =>
      val distanceToTemplate = frameDistance(logoFrame, logoTemplate)
      val logoMeanIntensity = meanLuminance(logoFrame)
      val cond1 = distanceToTemplate < threshold1
      val cond2 = (clusterCenter - threshold2) <= logoMeanIntensity
      val cond3 = logoMeanIntensity <= (clusterCenter + threshold2)
      println(s"Logo ${logoFrame.index} " +
        s"\tCond2 : ${clusterCenter - threshold2}" +
        s" \tTime :${(logoFrame.index/25/60).toInt}m ${(logoFrame.index/25%60).toInt}s " +
        s" \tDist: $distanceToTemplate\tC1:$cond1\tC2:$cond2\tC3:$cond3")
      cond1 & true & true
    }
  }
  
  
  def findLogoInVideo(): Unit = {
    val logos = findPotentialLogo
    val (filteredLogos, clusterCenter) = clusterizeLogos(logos)
    val (logoTemplate, distance) = findLogoTemplate(logos)
    println(s"Logo template at ${logoTemplate.index} " + s"\t${(logoTemplate.index/25/60).toInt}m ${(logoTemplate.index/25%60).toInt}")
    Imgcodecs.imwrite(videoPath + "_templ.jpg", logoTemplate.matrix)
    val matchedLogos = matchLogosToTemplate(logos, logoTemplate, clusterCenter)
    matchedLogos.foreach{l =>
      println("Logos at " + l.index + s"\t${(l.index/25/60).toInt}m ${(l.index/25%60).toInt}s")
    }
    findReplay(matchedLogos).foreach{replay =>
      val begin = replay.begin / 25
      val end = replay.end / 25
      println(s"Replay between ${(begin/60).toInt}m ${(begin%60).toInt}s and ${(end/60).toInt}m ${(end%60).toInt}s")
    }
  }
  
  
  private def index2frame(idxs: Seq[Int]): Seq[Frame] = {
    val frames = idxs.map{idx =>
      val frame = new Mat()
      capture.set(Videoio.CAP_PROP_POS_FRAMES, idx)
      capture.read(frame)
      Imgproc.resize(frame, frame, new Size(width, height))
      Frame(idx, frame)
    }
    frames
  }
  
  private def index2frameWindow(idxs: Seq[Int]): Seq[Seq[Frame]] = {
    val frames = idxs.map{idx =>
      val firstFrame = idx - windowSize
      capture.set(Videoio.CAP_PROP_POS_FRAMES, idx)
      (firstFrame until idx).map {id =>
        val frame = new Mat()
        capture.read(frame)
        Imgproc.resize(frame, frame, new Size(width, height))
        Imgproc.resize(frame, frame, new Size(width, height))
        Frame(id, frame)
      }
    }
    frames
  }
  
  private def frame2orb(frame: Frame) = {
    val kp = new MatOfKeyPoint()
    val desc = new Mat()
    orb.detect(frame.matrix, kp)
    orb.compute(frame.matrix, kp, desc)
    frame.copy(orbResult = Some(desc))
  }
  
  private def clusterizeFrames(frames: Seq[Frame], k: Int)= {
    val labels = new Mat()
    val centers = new Mat()
    val criteria = new TermCriteria(TermCriteria.EPS + TermCriteria.MAX_ITER, 100, 0.1)

    val disc = frames.flatMap {logo =>
      logo.orbResult
    }
    
    val colsLen = frames.head.orbResult.get.cols()
    val s = new Mat(frames.size, colsLen, CvType.CV_32F)
    val sample = new Mat()
    s.convertTo(sample, CvType.CV_32F)
  
    disc.foreach { d =>
      val row = new Mat()
      d.convertTo(row, CvType.CV_32F)
      sample.push_back(row)
    }
    println("Sorting " + frames.size + " frames")
    Core.kmeans(sample, k, labels, criteria, 3, Core.KMEANS_PP_CENTERS, centers)
    val indexedLabel = disc.indices.map {idx =>
      val predLabel = labels.get(idx, 0).head.toInt
      predLabel
    }
    
    frames.zip(indexedLabel).foreach {case (frame, l) =>
        println(s"${frame.index}\t$l")
    }
  }
  
  def findLogoInVideo(frameIdxs: Seq[Int]) = {
    val potentialLogos = index2frame(frameIdxs)
    val orbResult = potentialLogos.map(frame2orb)
    val cluster = clusterizeFrames(orbResult, 3)
  }
  
  /*
  def findLogoInVideo(frameIdxs: Seq[Int]): Unit = {
    val potentialLogos = index2frameWindow(frameIdxs).flatMap { window =>
      val mats = window.map(_.matrix).toList
      val lumi = accumulatedDiff(mats)
      if (lumi > luminanceThreshold) {
        println(s"${window.last.index} is a potential logo (lumi : $lumi)")
        Some(Frame(window.last.index, mats.last))
      }
      else None
    }
    //val potentialLogos = index2frame(frameIdxs)
    val (filteredLogos, clusterCenter) = clusterizeLogos(potentialLogos)
    val (logoTemplate, distance) = findLogoTemplate(filteredLogos)
    println(s"Logo template at ${logoTemplate.index} " + s"\t${(logoTemplate.index/25/60).toInt}m ${(logoTemplate.index/25%60).toInt}")
    Imgcodecs.imwrite(videoPath + "_templ.jpg", logoTemplate.matrix)
    val matchedLogos = matchLogosToTemplate(potentialLogos, logoTemplate, clusterCenter)
    matchedLogos.foreach{l =>
      println("Logos at " + l.index + s"\t${(l.index/25/60).toInt}m ${(l.index/25%60).toInt}s")
    }
    findReplay(matchedLogos).foreach{replay =>
      val begin = replay.begin / 25
      val end = replay.end / 25
      println(s"Replay between ${(begin/60).toInt}m ${(begin%60).toInt}s and ${(end/60).toInt}m ${(end%60).toInt}s")
    }
  }
  */
}

object LogoDetector {
  def apply(videoPath: String, frameToAnalyze: Int = Int.MaxValue): Unit = {
    val logoDetector = new LogoDetector(videoPath, frameToAnalyze)
    logoDetector.findLogoInVideo()
  }
  
  def apply(videoPath: String, keyframe: Seq[Int]): Unit = {
    val logoDetector = new LogoDetector(videoPath)
    logoDetector.findLogoInVideo(keyframe)
  }
}






*/
FROM debian

RUN mkdir /opencv
ADD install-opencv.sh /opencv
RUN chmod +x /opencv/install-opencv.sh
RUN ./opencv/install-opencv.sh

RUN mkdir /home/replay-detector
RUN mkdir /home/replay-detector/libs
WORKDIR /home/replay-detector
RUN cp /usr/local/share/OpenCV/java/opencv-342.jar libs
RUN cp /usr/local/share/OpenCV/java/libopencv_java342.so libs
ADD ReplayDetector.jar .
ADD scrap_video.sh .
RUN chmod +x scrap_video.sh

## youtube-dl
RUN wget https://yt-dl.org/latest/youtube-dl -O /usr/local/bin/youtube-dl
RUN chmod a+x /usr/local/bin/youtube-dl
RUN hash -r

CMD ["./scrap_video.sh", "ogxPw5x3mMY"]
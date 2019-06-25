FROM debian

RUN mkdir /opencv
ADD install-opencv.sh /opencv
RUN chmod +x /opencv/install-opencv.sh &\
    ./opencv/install-opencv.sh

RUN mkdir /home/replay-detector &\
    mkdir /home/replay-detector/libs
WORKDIR /home/replay-detector
RUN cp /usr/local/share/OpenCV/java/opencv-342.jar libs &\
    cp /usr/local/share/OpenCV/java/libopencv_java342.so libs
ADD ReplayDetector.jar .


CMD /bin/sh
#CMD java -jar ReplayDetector.jar
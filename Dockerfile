FROM debian:8
FROM openjdk:8

RUN mkdir /opencv
WORKDIR /opencv
ARG OPENCV_VERSION='3.4.2'
RUN apt-get -y update && \
    apt-get install -y build-essential cmake && \
    apt-get install -y qt5-default libvtk6-dev && \
    apt-get install -y zlib1g-dev libjpeg-dev libwebp-dev libpng-dev libtiff5-dev \
                            libopenexr-dev libgdal-dev && \
    apt-get install -y libdc1394-22-dev libavcodec-dev libavformat-dev libswscale-dev \
                            libtheora-dev libvorbis-dev libxvidcore-dev libx264-dev yasm \
                            libopencore-amrnb-dev libopencore-amrwb-dev libv4l-dev libxine2-dev && \
    apt-get install -y libtbb-dev libeigen3-dev && \
    apt-get install -y ant default-jdk && \
    apt-get install -y doxygen && \
    apt-get install -y unzip wget
RUN wget https://github.com/opencv/opencv/archive/${OPENCV_VERSION}.zip && \
    unzip ${OPENCV_VERSION}.zip && rm ${OPENCV_VERSION}.zip && \
    mv opencv-${OPENCV_VERSION} OpenCV && \
    cd OpenCV && mkdir build && cd build && \
    cmake -DWITH_QT=ON -DWITH_OPENGL=ON -DFORCE_VTK=ON -DWITH_TBB=ON -DWITH_GDAL=ON \
          -DWITH_XINE=ON -DBUILD_EXAMPLES=ON -DENABLE_PRECOMPILED_HEADERS=OFF \
          -DBUILD_SHARED_LIBS=OFF -D BUILD_NEW_PYTHON_SUPPORT=NO .. && \
    make -j4 && \
    make install && \
    ldconfig && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

## install youtube-dl
RUN wget https://yt-dl.org/latest/youtube-dl -O /usr/local/bin/youtube-dl
RUN chmod a+x /usr/local/bin/youtube-dl
RUN hash -r

# install curl
RUN apt-get install curl

# install pip
# Install dependencies
RUN apt-get update && apt-get install -y \
    python-pip

## install gsutil
#ARG CLOUD_SDK_VERSION=258.0.0
#ENV CLOUD_SDK_VERSION=$CLOUD_SDK_VERSION
#ENV PATH "$PATH:/opt/google-cloud-sdk/bin/"
#RUN apt-get -qqy update && apt-get install -qqy \
#        curl \
#        gcc \
#        python-dev \
#        python-setuptools \
#        apt-transport-https \
#        lsb-release \
#        openssh-client \
#        git \
#        gnupg \
#    && \
#    pip install -U crcmod   && \
#    export CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -c -s)" && \
#    echo "deb https://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" > /etc/apt/sources.list.d/google-cloud-sdk.list && \
#    curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - && \
#    apt-get update && \
#    apt-get install -y google-cloud-sdk=${CLOUD_SDK_VERSION}-0 \
#        google-cloud-sdk-app-engine-python=${CLOUD_SDK_VERSION}-0 \
#        google-cloud-sdk-app-engine-python-extras=${CLOUD_SDK_VERSION}-0 \
#        google-cloud-sdk-app-engine-java=${CLOUD_SDK_VERSION}-0 \
#        google-cloud-sdk-app-engine-go=${CLOUD_SDK_VERSION}-0 \
#        google-cloud-sdk-datalab=${CLOUD_SDK_VERSION}-0 \
#        google-cloud-sdk-datastore-emulator=${CLOUD_SDK_VERSION}-0 \
#        google-cloud-sdk-pubsub-emulator=${CLOUD_SDK_VERSION}-0 \
#        google-cloud-sdk-bigtable-emulator=${CLOUD_SDK_VERSION}-0 \
#        google-cloud-sdk-cbt=${CLOUD_SDK_VERSION}-0

# download sbt
RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list && \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 642AC823 && \
    apt-get update && \
    apt-get install sbt

ADD . /home/replay-detector/
WORKDIR /home/replay-detector

RUN cp /usr/local/share/OpenCV/java/opencv-342.jar lib
RUN cp /usr/local/share/OpenCV/java/libopencv_java342.so lib
ADD scrap_video.sh .
RUN chmod +x scrap_video.sh

RUN sbt assembly


# running the program
# CMD ["./scrap_video.sh", "ogxPw5x3mMY"]
EXPOSE 8080
ENTRYPOINT java -Djava.library.path=./lib -jar target/scala-2.12/ReplayDetector-assembly-0.1.jar
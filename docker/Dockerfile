FROM debian
FROM openjdk:8

RUN mkdir /opencv
ADD install-opencv.sh /opencv
RUN chmod +x /opencv/install-opencv.sh
RUN ./opencv/install-opencv.sh

RUN mkdir /home/replay-detector
RUN mkdir /home/replay-detector/libs
WORKDIR /home/replay-detector
RUN cp /usr/local/share/OpenCV/java/opencv-342.jar libs
RUN cp /usr/local/share/OpenCV/java/libopencv_java342.so libs
ADD scrap_video.sh .
RUN chmod +x scrap_video.sh

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
ARG CLOUD_SDK_VERSION=258.0.0
ENV CLOUD_SDK_VERSION=$CLOUD_SDK_VERSION
ENV PATH "$PATH:/opt/google-cloud-sdk/bin/"
RUN apt-get -qqy update && apt-get install -qqy \
        curl \
        gcc \
        python-dev \
        python-setuptools \
        apt-transport-https \
        lsb-release \
        openssh-client \
        git \
        gnupg \
    && \
    pip install -U crcmod   && \
    export CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -c -s)" && \
    echo "deb https://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" > /etc/apt/sources.list.d/google-cloud-sdk.list && \
    curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - && \
    apt-get update && \
    apt-get install -y google-cloud-sdk=${CLOUD_SDK_VERSION}-0 \
        google-cloud-sdk-app-engine-python=${CLOUD_SDK_VERSION}-0 \
        google-cloud-sdk-app-engine-python-extras=${CLOUD_SDK_VERSION}-0 \
        google-cloud-sdk-app-engine-java=${CLOUD_SDK_VERSION}-0 \
        google-cloud-sdk-app-engine-go=${CLOUD_SDK_VERSION}-0 \
        google-cloud-sdk-datalab=${CLOUD_SDK_VERSION}-0 \
        google-cloud-sdk-datastore-emulator=${CLOUD_SDK_VERSION}-0 \
        google-cloud-sdk-pubsub-emulator=${CLOUD_SDK_VERSION}-0 \
        google-cloud-sdk-bigtable-emulator=${CLOUD_SDK_VERSION}-0 \
        google-cloud-sdk-cbt=${CLOUD_SDK_VERSION}-0

# copy the jar
ADD ReplayDetector.jar .

# running the program
# CMD ["./scrap_video.sh", "ogxPw5x3mMY"]
EXPOSE 8080
ENTRYPOINT java -Djava.library.path=./libs -jar ReplayDetector.jar
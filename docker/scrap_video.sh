#!/bin/bash

YOUTUBE_URL="https://www.youtube.com/watch?v=$1"
echo $1 >> parsed
youtube-dl -f 160 $YOUTUBE_URL -o video.mp4
java -Djava.library.path=./libs -jar ReplayDetector.jar video.mp4

gsutil cp -r frame gs://logo_detection_shots
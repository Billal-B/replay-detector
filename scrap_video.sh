#!/bin/bash
YOUTUBE_URL="https://www.youtube.com/watch?v=$1"
echo $1 >> parsed
youtube-dl -f 160 $YOUTUBE_URL -o video.mp4
java -Djava.library.path=./lib -jar ReplayDetector.jar

install-open-cv:
	./install-opencv.sh
	cp /usr/local/share/OpenCV/java/opencv-342.jar lib
	cp /usr/local/share/OpenCV/java/libopencv_java342.so lib

install-youtube-dl:
	wget https://yt-dl.org/latest/youtube-dl -O /usr/local/bin/youtube-dl
	chmod a+x /usr/local/bin/youtube-dl
	hash -r

build:
	sbt assembly
	mv target/scala-2.12/ReplayDetector* ReplayDetector.jar

docker-build:
	docker build -t replay-detector:latest .

run:
	MODE=YOUTUBE java -Djava.library.path=./lib -jar ReplayDetector.jar
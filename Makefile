install-open-cv:
	$(shell docker/install-opencv.sh)

build:
	sbt assembly
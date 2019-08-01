REM ogxPw5x3mMY
set replay_detector_url=https://www.youtube.com/watch?v=%1
ECHO %1>> parsed
REM del "video.mp4"

set replay_detector_filename="video.mp4"
REM youtube-dl.exe -f 160 %replay_detector_url% -o %replay_detector_filename%
java.exe -Djava.library.path=./lib -jar ReplayDetector.jar %replay_detector_filename%
REM ogxPw5x3mMY
set replay_detector_url=https://www.youtube.com/watch?v=%1
ECHO %1>> parsed
del "video.mp4"

set replay_detector_filename="video.mp4"
youtube-dl.exe -f 160 %replay_detector_url% -o %replay_detector_filename%
java.exe -jar ReplayDetector.jar %replay_detector_filename%
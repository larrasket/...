cd "/home/ghd/me/cinema/Shows/Breaking Bad/S02"
mkdir .cut_video
ffmpeg -ss 2193.149 -i /home/ghd/me/cinema/Shows/Breaking\ Bad/S02/Breaking\ Bad\ s02ep3\ 720p\ brrip.sujaidr.mkv -t 83.125 -c  copy -avoid_negative_ts 1 .cut_video/clip0.mkv
echo "file 'clip0.mkv'" >>.cut_video/concat.txt
cp .cut_video/clip0.mkv "`date "+%Y_%m_%d_%H_%M_%S"`_cut_Breaking Bad s02ep3 720p brrip.sujaidr.mkv"
rm -rf .cut_video
echo OK!
echo srcipt_dir:/home/ghd/.config/mpv/scripts

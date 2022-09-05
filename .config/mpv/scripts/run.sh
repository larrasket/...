cd "/home/ghd/me/cinema/Movies/Blue Valentine (2010)"
mkdir .cut_video
ffmpeg -ss 1167.249 -i /home/ghd/me/cinema/Movies/Blue\ Valentine\ \(2010\)/Blue.Valentine.BrRip.720p.x264.YIFY.mkv -t 35.244 -c  copy -avoid_negative_ts 1 .cut_video/clip0.mkv
echo "file 'clip0.mkv'" >>.cut_video/concat.txt
cp .cut_video/clip0.mkv "`date "+%Y_%m_%d_%H_%M_%S"`_cut_Blue.Valentine.BrRip.720p.x264.YIFY.mkv"
rm -rf .cut_video
echo OK!
echo srcipt_dir:/home/ghd/.config/mpv/scripts

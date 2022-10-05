cd "/home/ghd/me/cinema/Shows/Black Mirror/S03"
mkdir .cut_video
ffmpeg -ss 2855.083 -i /home/ghd/me/cinema/Shows/Black\ Mirror/S03/Black.Mirror.S03E03.720p.BluRay.x264-GalaxyTV.mkv -t 252.000 -c  copy -avoid_negative_ts 1 .cut_video/clip0.mkv
echo "file 'clip0.mkv'" >>.cut_video/concat.txt
cp .cut_video/clip0.mkv "`date "+%Y_%m_%d_%H_%M_%S"`_cut_Black.Mirror.S03E03.720p.BluRay.x264-GalaxyTV.mkv"
rm -rf .cut_video
echo OK!
echo srcipt_dir:/home/ghd/.config/mpv/scripts

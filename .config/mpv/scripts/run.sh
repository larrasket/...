cd "/home/ghd/me/cinema/Movies/Jojo Rabbit (2019) "
mkdir .cut_video
ffmpeg -ss 145.6455 -i /home/ghd/me/cinema/Movies/Jojo\ Rabbit\ \(2019\)\ /Jojo.Rabbit.2019.720p.WEBRip.x264.AAC-\[YTS.MX\].mp4 -t 14.93158333333 -c  copy -avoid_negative_ts 1 .cut_video/clip0.mp4
echo "file 'clip0.mp4'" >>.cut_video/concat.txt
cp .cut_video/clip0.mp4 "`date "+%Y_%m_%d_%H_%M_%S"`_cut_Jojo.Rabbit.2019.720p.WEBRip.x264.AAC-[YTS.MX].mp4"
rm -rf .cut_video
echo OK!
echo srcipt_dir:/home/ghd/.config/mpv/scripts

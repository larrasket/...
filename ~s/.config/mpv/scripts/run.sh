cd "/home/ghd/me/cinema/Shows/Ragel W Set Setat (2006)/S01"
mkdir .cut_video
ffmpeg -ss 4.88 -i /home/ghd/me/cinema/Shows/Ragel\ W\ Set\ Setat\ \(2006\)/S01/Ragel.W.Set.Setat.2006.S01Ep01.720p.WEB-DL.akwam.io.mp4 -t  -c  copy -avoid_negative_ts 1 .cut_video/clip0.mp4
echo "file 'clip0.mp4'" >>.cut_video/concat.txt
cp .cut_video/clip0.mp4 "`date "+%Y_%m_%d_%H_%M_%S"`_cut_Ragel.W.Set.Setat.2006.S01Ep01.720p.WEB-DL.akwam.io.mp4"
rm -rf .cut_video
echo OK!
echo srcipt_dir:/home/ghd/.config/mpv/scripts

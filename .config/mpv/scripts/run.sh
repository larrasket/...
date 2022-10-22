cd "/home/ghd/me/cinema/Movies/archive/Being There (1979)"
mkdir .cut_video
ffmpeg -ss 5340.12 -i /home/ghd/me/cinema/Movies/archive/Being\ There\ \(1979\)/Being.There.1979.512x288.25fps.689kbs.88mp3.MultiSub.WunSeeDee.avi -t 46.28 -c  copy -avoid_negative_ts 1 .cut_video/clip0.avi
echo "file 'clip0.avi'" >>.cut_video/concat.txt
cp .cut_video/clip0.avi "`date "+%Y_%m_%d_%H_%M_%S"`_cut_Being.There.1979.512x288.25fps.689kbs.88mp3.MultiSub.WunSeeDee.avi"
rm -rf .cut_video
echo OK!
echo srcipt_dir:/home/ghd/.config/mpv/scripts

cd "/home/ghd/me/cinema/Shows/Better Call Saul/S05"
mkdir .cut_video
ffmpeg -f concat -i .cut_video/concat.txt -c copy "`date "+%Y_%m_%d_%H_%M_%S"`_cut_Better.Call.Saul.S05E06.720p.NF.WEBRip.x264-GalaxyTV.mkv"
rm -rf .cut_video
echo OK!
echo srcipt_dir:/home/ghd/.config/mpv/scripts

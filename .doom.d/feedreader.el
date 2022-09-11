;;; ../configs/.doom.d/feedreader.el -*- lexical-binding: t; -*-
(add-hook! 'elfeed-search-mode-hook #'elfeed-update)
(add-hook 'elfeed-search-mode-hook
      (lambda () (setq elfeed-feeds
                  '(("http://www.ahl-ul-bayt.org/ar/?format=feed&type=rss" art)
                    ("https://www.reddit.com/r/cscareerquestions/.rss" cs)
                    ("https://anime--irl.tumblr.com/rss" comics)
                    ("https://www.reddit.com/r/Semenretention/.rss" people reddit)
                    ("https://xkcd.com/rss.xml" comics)
                    ("https://www.reddit.com/r/awakened/.rss" people reddit)
                    ("https://www.reddit.com/r/MathJokes/.rss" reddit)
                    ("https://www.reddit.com/r/Egypt/.rss" people reddit)
                    ("https://www.reddit.com/r/slatestarcodex/.rss" theory reddit)
                    ("https://www.reddit.com/r/theoryofpropaganda/.rss" theory reddit)
                    ("https://revsoc.me/feed" theory)
                    ("https://www.ahewar.org/rss/default.asp?lt=3&i=5329" theory)
                    ("https://www.ahewar.org/rss/default.asp?lt=3&i=1781" theory)
                    ("https://www.ahewar.org/rss/default.asp?lt=3&i=9391" theory)
                    ("https://naeem-writing.com/?feed=rss2" theory people)
                    ("https://www.ahewar.org/rss/default.asp?lt=5" theory)
                    ("https://engineering.linkedin.com/blog.rss.html" cs)
                    ("https://www.claudiobernasconi.ch/feed/" cs)
                    ("https://www.reddit.com/r/programming/.rss" cs)
                    ("https://www.fluentcpp.com//feed" cs) ;; Oh yeah! I miss those days I used to love C++.
                    ("https://www.cppstories.com/index.xml" cs)
                    ("https://thomaslevesque.com/index.xml" cs)
                    ("https://devblogs.microsoft.com/dotnet/feed/" cs)
                    ("https://feed.infoq.com/" cs software_engineering)
                    ("https://www.reddit.com/r/golang/.rss" cs)
                    ("https://suckless.org/atom.xml" cs)
                    ("https://blog.janestreet.com/feed.xml" cs)
                    ("https://www.ribice.ba/index.xml" cs)
                    ("https://github.com/salehmu.private.atom?token=ARVKOP6Q6D3TE577LYBCYSWA3RAUG" github)
                    ("http://www.infoworld.com/index.rss" cs)
                    ("https://hackernoon.com/feed" cs)
                    ("http://cachestocaches.com/feed/" cs)
                    ("https://boards.4channel.org/g/index.rss" cs)
                    ("https://boards.4channel.org/adv/index.rss" people)
                    ("https://harian-oftheday.blogspot.com/feeds/posts/default?alt=rss" spirit)
                    ("https://medium.com/feed/@villekuosmanen" cs)
		    ("https://www.reddit.com/r/OneTruthPrevails/.rss" reddit)
                    ("http://nedroid.com/feed/" comics)))
        (elfeed-update)))
(add-hook! 'elfeed-search-mode-hook #'elfeed-update)

(use-package elfeed-dashboard
  :load-path "~/gits/elfeed-dashboard/"
  :config
  (setq elfeed-dashboard-file "~/gits/elfeed-dashboard/elfeed-dashboard.org")
  ;; update feed counts on elfeed-quit
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

(provide 'feedreader)

;;; configs/~s/.doom.d/feeds.el -*- lexical-binding: t; -*-


(setq elfeed-search-feed-face ":foreground #fff :weight bold"
      elfeed-feeds '(("https://lukesmith.xyz/index.xml" philosophy)
                     ("http://monakareem.blogspot.com/feeds/posts/default?alt=rss" blogs)
                     ("https://blackaly.github.io//feed.xml" blogs)
                     ("https://sachachua.com/blog/category/monthly/feed" blogs)
                     ("https://www.emadelsaid.com/+/feed.rss" blogs)
                     ("https://appliedgo.net/index.xml" programming)
                     ("https://williamdavies.blog/feed/" philosophy)
                     ("http://chomsky.info/feed/" philosophy)
                     ("https://blog.giovanh.com/feeds/atom.xml" cs)

                     ;; ("https://boards.4channel.org/sci/index.rss" science)
                     ;; ("https://stallman.org/rss/rss.xml" politics)
                     ("https://old.reddit.com/r/emacs/.rss" emacs)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCdHT7KB1gDAXZYpPW71fn0Q" yt philosophy)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA" yt cs)

                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVpb6nv1igxnoY0SRVhaoEA" yt cs)

                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC_5ENzPnzYCGRxiOIdtaRug" yt aesthetic)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC99wd4wi8AfaSkqprWo206g" yt aesthetic)
                     ;; ("https://theconversation.com/africa/home-page/news.atom" politics)
                     ;; ("https://old.reddit.com/r/pureretention/.rss" social)

                     ;; ("https://stackexchange.com/feeds/questions" cs)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCoxcjq-8xIDTYp3uz647V5A" yt math)))
                     ;; ("https://www.nature.com/palcomms.rss" science)))


(use-package elfeed-goodies
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-size 0.5))


(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread"))

(add-hook 'elfeed-search-mode-hook #'elfeed-update)

(provide '+feeds)

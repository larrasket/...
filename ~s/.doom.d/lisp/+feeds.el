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

                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCdHT7KB1gDAXZYpPW71fn0Q" yt philosophy)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA" yt cs)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVpb6nv1igxnoY0SRVhaoEA" yt cs)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC_5ENzPnzYCGRxiOIdtaRug" yt aesthetic)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC99wd4wi8AfaSkqprWo206g" yt aesthetic)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCoxcjq-8xIDTYp3uz647V5A" yt math)))


(use-package elfeed-goodies
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-size 0.5))


(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread"))

(add-hook 'elfeed-search-mode-hook #'elfeed-update)



(defun salih/add-feeds-by-keyword (keyword-category-list)
  (let ((urls (list "https://hnrss.org/newest?q=%s"
                    "https://rssc.fly.dev/rss?src=https://boards.4channel.org/sci/index.rss&titlef=(?i)%s&descriptionf=(?i)%s"
                    "https://rssc.fly.dev/rss?src=https://boards.4channel.org/g/index.rss&titlef=(?i)%s&descriptionf=(?i)%s"
                    "https://rssc.fly.dev/rss?src=https://theconversation.com/articles.atom?language=en&titlef=(?i)%s&descriptionf=(?i)%s")))
    (dolist (pair keyword-category-list)
      (let ((keyword (car pair))
            (category (cdr pair)))
        (dolist (url urls)
          (let ((l (salih/format url keyword)))
            (add-to-list 'elfeed-feeds `(,l ,category))))))))

(setq keyword-category-list
      '(("chomsky"      . philosophy)
        ("emacs"        . emacs)
        ("Egypt"        . politics)
        ("Algeria"      . politics)
        ("lisp"         . programming)
        ("Arab"         . politics)
        ("Israel"       . politics)
        ("Palestine"    . politics)))

(salih/add-feeds-by-keyword keyword-category-list)


(provide '+feeds)

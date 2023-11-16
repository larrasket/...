;;; configs/~s/.doom.d/feeds.el -*- lexical-binding: t; -*-


(setq elfeed-search-feed-face ":foreground #fff :weight bold"
      elfeed-feeds '(("https://michael.orlitzky.com/articles/rss.xml"                                           philosophy math computers)
                     ("http://www.mccaughan.org.uk/g/log/index.rss"                                             Gareth)
                     ("http://norvig.com/rss-feed.xml"                                                          norvig philosophy cs)
                     ("https://lukesmith.xyz/index.xml"                                                         luke philosophy)
                     ("http://monakareem.blogspot.com/feeds/posts/default?alt=rss"                              mona blogs)
                     ("https://blackaly.github.io//feed.xml"                                                    ali blogs)
                     ("https://sachachua.com/blog/category/monthly/feed"                                        emacs_asian_girl blogs)
                     ("https://www.emadelsaid.com/+/feed.rss"                                                   emad blogs)
                     ("https://appliedgo.net/index.xml"                                                         appliedgo programming)
                     ("https://williamdavies.blog/feed/"                                                        consumption philosophy)
                     ("http://chomsky.info/feed/"                                                               chomsky philosophy)
                     ("https://blog.giovanh.com/feeds/atom.xml"                                                 g cs)
                     ("https://daily.jstor.org//feed"                                                           jstore science)
                     ("https://protesilaos.com/master.xml"                                                      prot blogs emacs philosophy)
                     ("https://aeon.co/feed.rss"                                                                aeon philosophy)
                     ("https://psyche.co/feed.rss"                                                              psyche psychology)
                     ("https://www.radicalphilosophy.com/feed"                                                  radical_philosophy philosphy)
                     ("https://rssc.fly.dev/rss?src=https://www.alaraby.co.uk/rss&linkf=opinion"                alaraby)
                     ("https://nintil.com/rss.xml"                                                              nintil history)
                     ("https://stallman.org/rss/rss.xml"                                                        news rms)
                     ("https://www.theonion.com/rss"                                                            news onion)
                     ("https://www.econlib.org/feed/"                                                           economics econlib)
                     ("https://stackexchange.com/feeds/tagsets/450778/islam?sort=active"                        Islam   sx)
                     ("https://hnrss.org/replies?id=lr0"                                                        hn lr0)
                     ("https://stackexchange.com/feeds/tagsets/450949/interpersonal?sort=active"                interpersonal sx)
                     ;; youtube
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCGVHC4L6gjS13AMe-JMOjHg"            youtube kanb philosophy)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtx9oIT_eWp6jIkoULS-ZdQ"            youtube shabaka)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-pn1lcD--68WjOgCGBXlCQ"            youtube tasneem)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtscFf8VayggrDYjOwDke_Q"            youtube academia)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCij1VEjDb88RCbqsvZ1gqaw"            youtube sideof)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCmHu591mWNj_zSaSuYVwsaQ"            youtube manufacturing_intellect philosophy)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCX2U8kCH2EzKSeaTIJBtkkQ"            youtube ryan cs)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCT6H50SccbeQKlhHufvAo1A"            youtube prof)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCHv1q35tKUJiSWO9W2OQRhw"            youtube age)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCGVHC4L6gjS13AMe-JMOjHg"            youtube kane)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCUMZ7gohGI9HcU9VNsr2FJQ"            youtube bloomberg)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkS_HP3m9NXOgswVAKbMeJQ"            youtube thenandnow philosophy)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ"            youtube systemcraft emacs)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA"            youtube mentaloutlaw cs)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCX7katl3DVmch4D7LSvqbVQ"            youtube montemayor history)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCEqNbbsx0i7fhwRt0saYIcQ"            youtube america history)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7pr_dQxm2Ns2KlzRSx5FZA"            youtube sandrhoman history)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCrI5U0R293u9uveijefKyAA"            youtube ryan history)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCdHT7KB1gDAXZYpPW71fn0Q"            youtube Distributist philosophy)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqBiWcuTF8IaLH7wBqnihsQ"            youtube toldinstone history)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsaViv1SDQy2FcRFHrf_Dkw"            youtube hikma history)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA"            youtube computerphile cs)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCheAUBrk8xw6QhJIxEPsvhg"            youtube thelifeguide history)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9ff15w4ufviWfv9UfIuByA"            youtube wes cecil history philosophy)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMhLD2vecQSBqKd5uVPrwaQ"            youtube historiancraft history)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCp9ZtilfKJds0iWytR_pnOQ"            youtube Epimetheum history)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVpb6nv1igxnoY0SRVhaoEA"            youtube engieering cs)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC_5ENzPnzYCGRxiOIdtaRug"            youtube ramah aesthetic)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC99wd4wi8AfaSkqprWo206g"            youtube edits aesthetic)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCoxcjq-8xIDTYp3uz647V5A"            youtube numberphile math)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtYLUTtgS3k1Fg4y5tAhLbw"            youtube statquest math)))


(defvar salih/common-feeds
  (list "https://hnrss.org/newest?q=%s"
        "https://stackexchange.com/feeds/tagsets/450777/politics?sort=active"
        "https://rssc.fly.dev/rss?src=https://www.reddit.com/r/programming/.rss&titlef=(?i)%s&descriptionf=(?i)%s"
        "https://rssc.fly.dev/rss?src=https://boards.4channel.org/g/index.rss&titlef=(?i)%s&descriptionf=(?i)%s"
        "https://rssc.fly.dev/rss?src=https://theconversation.com/articles.atom?language=en&titlef=(?i)%s&descriptionf=(?i)%s"))


(defvar salih/stack-list
  (concat "stackoverflow\\.com"                             "|"
          "stats\\.stackexchange\\.com"                     "|"
          "math\\.stackexchange\\.com"                      "|"
          "askubuntu\\.com"                                 "|"
          "academia\\.stackexchange\\.com"                  "|"
          "money\\.stackexchange\\.com"                     "|"
          "english\\.stackexchange\\.com"                   "|"
          "chess\\.stackexchange\\.com"                     "|"
          "cs\\.stackexchange\\.com"                        "|"
          "emacs\\.stackexchange\\.com"                     "|"
          "ell\\.stackexchange\\.com"                       "|"
          "history\\.stackexchange\\.com"                   "|"
          "hsm\\.stackexchange\\.com"                       "|"
          "islam\\.stackexchange\\.com"                     "|"
          "linguistics\\.stackexchange\\.com"               "|"
          "literature\\.stackexchange\\.com"                "|"
          "mathoverflow\\.net"                              "|"
          "philosophy\\.stackexchange\\.com"                "|"
          "politics\\.stackexchange\\.com"                  "|"
          "softwareengineering\\.stackexchange\\.com"       "|"
          "superuser\\.com"                                 "|"
          "unix\\.stackexchange\\.com"                      "|"
          "vi\\.stackexchange\\.com"))


(add-to-list 'elfeed-feeds `(,(format "https://rssc.fly.dev/rss?src=https://stackexchange.com/feeds/questions&titlef=%s" salih/stack-list) questions))

(use-package elfeed-goodies
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-size 0.5))


(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread"))

(add-hook 'elfeed-search-mode-hook #'elfeed-update)



(defun salih/add-feeds-by-keyword (salih/keyword-category-list)
  (let ((urls salih/common-feeds))
    (dolist (pair salih/keyword-category-list)
      (let ((keyword (car pair))
            (category (cdr pair)))
        (dolist (url urls)
          (let ((l (salih/format url keyword)))
            (add-to-list 'elfeed-feeds `(,l ,category))))))))




(defvar salih/keyword-category-list
  '(("chomsky"      . philosophy)
    ("arabic"       . philosophy)
    ("emacs"        . emacs)
    ("Egypt"        . politics)
    ("Algeria"      . politics)
    ("lisp"         . programming)
    ("julie"        . programming)
    ("Arab"         . politics)
    ("Palestine"    . politics)
    ("Israel"       . politics)
    ("linguistics"  . philosophy)
    ("linguist"     . philosophy)))

(salih/add-feeds-by-keyword salih/keyword-category-list)


(provide '+feeds)

;; Local Variables:
;; eval: (auto-fill-mode -1)
;; End:

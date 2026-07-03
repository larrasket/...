;;; lr-elfeed.el --- Elfeed RSS reader: feeds and tags -*- lexical-binding: t; -*-

;;; --- Org capture: store the article URL (not an elfeed: link) and capture

(defun salih/elfeed--current-entry ()
  "Return the Elfeed entry at point, in either search or show buffers."
  (cond
   ((derived-mode-p 'elfeed-show-mode) elfeed-show-entry)
   ((derived-mode-p 'elfeed-search-mode)
    (let ((sel (elfeed-search-selected t)))
      (if (consp sel) (car sel) sel)))))

(defun salih/elfeed-org-store-and-capture ()
  "Store an Org link to the current entry's article URL and capture.
Unlike the built-in `elfeed' Org link (which stores an `elfeed:' link),
this stores the real http(s) URL with the entry title as the description,
then opens `org-capture' with template \"f\".  This is the Elfeed twin of
`salih/mu4e-org-store-and-capture'."
  (interactive)
  (require 'org)
  (let ((entry (salih/elfeed--current-entry)))
    (unless entry (user-error "No Elfeed entry at point"))
    (let* ((url   (elfeed-entry-link entry))
           (title (or (elfeed-entry-title entry) url)))
      (unless url (user-error "Entry has no URL"))
      (org-store-link-props :type "http" :link url :description title)
      (push (list url title) org-stored-links)
      (org-capture nil "f"))))

;;; --- Search ordering: cluster entries into stable, contiguous groups
;;
;; Goal: every entry from the same source (or author) sits together, and the
;; order is *deterministic* — pressing "r"/"g" never reshuffles the buffer, and
;; a background fetch only slots new entries into their existing group instead
;; of churning everything.  The previous version ordered groups by "freshest
;; author first", i.e. by date, so reading an entry or fetching new ones changed
;; which author was freshest and reshuffled the whole list.  We drop recency
;; from the *group* ordering entirely: groups are ordered alphabetically by a
;; stable key, and the comparator is a *total* order (final tiebreak on the
;; entry id) so the result is identical no matter what order
;; `elfeed-search--update-list' hands us.

(defvar salih/elfeed-group-by 'source
  "How to cluster entries in the elfeed search buffer.
`source' groups by feed title (all of a site's posts together); `author'
groups by per-entry author metadata, falling back to the feed title when an
entry has none.  `source' avoids scattering aggregator feeds (HN, Reddit,
Lobsters) whose entries each carry a different submitter as the author.")

(defun salih/elfeed-entry-group-key (entry)
  "Stable grouping key for ENTRY: a feed title or author name.
Honours `salih/elfeed-group-by'.  Never returns nil, so entries with missing
metadata still cluster together deterministically."
  (let ((feed-title (or (elfeed-meta--title (elfeed-entry-feed entry)) "")))
    (if (eq salih/elfeed-group-by 'author)
        (let* ((meta (car (elfeed-meta entry :authors)))
               (name (and meta (plist-get meta :name))))
          (or name feed-title))
      feed-title)))

(defun salih/elfeed-entry-id-string (entry)
  "Deterministic unique string for ENTRY, used as a final sort tiebreak."
  (let ((id (elfeed-entry-id entry)))   ; (FEED-ID . ENTRY-ID), both strings
    (concat (car id) "\0" (cdr id))))

(defun salih/elfeed-entry-lessp (x y)
  "Total-order predicate: cluster by group, newest within a group.
Groups are ordered alphabetically by `salih/elfeed-entry-group-key' — a key
that depends on neither dates nor read state, so the ordering is stable across
refreshes and background fetches.  Within a group the newest entry comes
first; exact-date ties break on the entry id.  Being a total order, this
yields the same result regardless of the input order."
  (let ((gx (salih/elfeed-entry-group-key x))
        (gy (salih/elfeed-entry-group-key y)))
    (if (not (string= gx gy))
        (string-collate-lessp gx gy nil t)
      (let ((dx (elfeed-entry-date x))
            (dy (elfeed-entry-date y)))
        (if (/= dx dy)
            (> dx dy)                    ; newest first within a group
          (string< (salih/elfeed-entry-id-string x)
                   (salih/elfeed-entry-id-string y)))))))

(defun salih/elfeed-cluster-entries (&rest _)
  "Re-order `elfeed-search-entries' into deterministic, contiguous groups.
Installed as :after advice on `elfeed-search--update-list' because a pairwise
`elfeed-search-sort-function' is consulted per-entry and cannot guarantee a
stable global clustering.  See `salih/elfeed-entry-lessp'.  `advice-add' is
idempotent, so re-evaluating this file won't stack duplicate advice."
  (when elfeed-search-entries
    (setq elfeed-search-entries
          (sort elfeed-search-entries #'salih/elfeed-entry-lessp))))


;;; --- Elfeed proper

(after! elfeed
  (setq elfeed-db-directory (expand-file-name "elfeed/" doom-cache-dir)
        elfeed-search-filter "@2-weeks-ago +unread"
        elfeed-search-title-max-width 100
        elfeed-search-title-min-width 30)

  ;; Compatibility shim.  `elfeed-goodies/search-header-draw' (installed as
  ;; `elfeed-search-header-function' by `elfeed-goodies/setup') calls
  ;; `elfeed-search--intro-header' whenever the database has never been
  ;; updated, i.e. `(zerop (elfeed-db-last-update))'.  We track the latest
  ;; elfeed via `unpin!', and current elfeed no longer defines that function,
  ;; so opening elfeed before the first fetch throws `void-function
  ;; elfeed-search--intro-header' on every redisplay.  Fall back to elfeed's
  ;; standard header so the search buffer renders regardless.
  (unless (fboundp 'elfeed-search--intro-header)
    (defun elfeed-search--intro-header ()
      "Compatibility shim for `elfeed-goodies'; see `lr-elfeed.el'."
      (if (fboundp 'elfeed-search--header)
          (elfeed-search--header)
        "")))

  ;; Cluster the search list into stable, contiguous groups via an :after pass.
  ;; We disable the built-in pairwise sort and let the advice reorder the
  ;; already-built list with a total-order comparator (see
  ;; `salih/elfeed-cluster-entries').  `advice-add' is idempotent; we also drop
  ;; the previous advice function so re-evaluating after a rename doesn't leave
  ;; the old date-based clustering installed alongside the new one.
  (setq elfeed-search-sort-function nil)
  (advice-remove 'elfeed-search--update-list 'salih/elfeed-cluster-by-author)
  (advice-add 'elfeed-search--update-list :after #'salih/elfeed-cluster-entries)
  ;; If a search buffer is already open (e.g. after `doom/reload'), drop any
  ;; stale buffer-local sort and re-sort now so the new ordering shows
  ;; immediately instead of only on the next refresh.
  (when-let* ((buf (get-buffer "*elfeed-search*")))
    (with-current-buffer buf
      (kill-local-variable 'elfeed-search-sort-function)
      (when (derived-mode-p 'elfeed-search-mode)
        (elfeed-search-update :force))))

  (setq elfeed-feeds
        '(;; ---------- Programming — personal blogs ----------
          ("https://blog.giovanh.com/feeds/atom.xml"            blog programming)
          ("https://cedwards.xyz/index.xml"                      blog programming)
          ("http://norvig.com/rss-feed.xml"                      blog programming)
          ("https://lukesmith.xyz/index.xml"                     blog)
          ("https://drewdevault.com/blog/index.xml"              blog programming)
          ("https://williamdavies.blog/feed/"                    blog)
          ("https://www.n16f.net/blog/index.xml"                 blog programming)
          ("https://rss.gabiseabra.dev/feed"                     blog programming)
          ("https://lewiscampbell.tech/blog/feed.xml"            blog programming)
          ("https://abhinavg.net/posts/index.xml"                blog programming)
          ("https://ploum.net/atom_en.xml"                       blog programming)
          ("https://maurycyz.com/index.xml"                      blog programming)
          ("https://rahim.li/index.xml"                          blog programming)
          ("https://gallant.dev/feeds/"                          blog)
          ("https://danluu.com/atom.xml"                         blog programming)
          ("https://martinalderson.com/feed.xml"                 blog programming ai)
          ("https://stephenramsay.net/rss.xml"                   blog programming)
          ("https://brooker.co.za/blog/atom.xml"                 blog programming)
          ("https://kevincox.ca/feed.atom"                       blog programming)
          ("http://boxbase.org/feed.rss"                         blog programming)
          ("https://nsrip.com/feed.xml"                          blog programming)
          ("https://alexanderdanilov.dev/rss.xml"                blog programming)
          ("https://blog.alexbeals.com/feeds/rss"                blog programming)
          ("https://utcc.utoronto.ca/~cks/space/blog/?atom"      blog programming)
          ("https://www.gleech.org/feed.xml"                     blog)
          ("https://kevinboone.me/feed.xml"                      blog programming)
          ("https://meyerweb.com/feed/"                          blog web)
          ("https://maia.crimew.gay/feed.xml"                    blog programming)
          ("https://third-bit.com/atom.xml"                      blog programming)
          ("https://www.pixelbeat.org/feed/rss2.xml"             blog programming)
          ("https://joshblais.com/index.xml"                     blog)
          ("https://aliquote.org/index.xml"                      blog programming)
          ("https://www.cyberdemon.org/feed.xml"                 blog programming)
          ("https://bobbyhiltz.com/rss.xml"                      blog)
          ("https://blog.avas.space/feed/"                       blog)
          ("https://notes.jeddacp.com/feed/"                     blog photography)
          ("https://www.fromjason.xyz/p/freelance/feed/feed.xml" blog)
          ("https://liamrosen.com/feed/"                         blog)
          ("https://trueblue.bearblog.dev/feed/?type=rss"        blog)
          ("https://takenvaullt.bearblog.dev/feed/?type=rss"     blog)
          ("https://atelfo.github.io/feed.xml"                   blog science)
          ("https://lr0.org/index.xml"                           blog programming)
          ("https://www.computerenhance.com/feed"                blog programming)
          ("https://appliedgo.net/index.xml"                     blog programming golang)
          ("https://words.filippo.io/rss/"                       blog programming security)
          ("https://nullpt.rs/feed.rss"                          blog programming security)
          ("https://www.emadelsaid.com/+/feed.rss"               blog programming arabic)

          ;; ---------- Programming — engineering & industry ----------
          ("https://engineering.fb.com/feed/"                    engineering programming)
          ("https://netflixtechblog.com/feed"                    engineering programming)
          ("https://dropbox.tech/feed"                           engineering programming)
          ("https://tailscale.com/blog/index.xml"                engineering programming)
          ("https://careersatdoordash.com/engineering-blog/feed/" engineering programming)
          ("https://engineering.atspotify.com/feed"              engineering programming)
          ("https://stackoverflow.blog/feed"                     programming)
          ("https://cprss.s3.amazonaws.com/golangweekly.com.xml" programming golang newsletter)

          ;; ---------- Emacs ----------
          ("https://kelar.org/~bandali/rss20.xml"                blog emacs)
          ("https://thanosapollo.org/posts/index.xml"            blog emacs)
          ("https://themkat.net/feed.xml"                        blog emacs)
          ("https://alhassy.com/rss.xml"                         blog emacs)
          ("https://joyofsource.com/feed.xml"                    blog emacs)
          ("https://endlessparentheses.com/atom.xml"             blog emacs)
          ("https://emacsconf.org/index.atom"                    emacs)

          ;; ---------- AI ----------
          ("https://erichartford.com/rss.xml"                    blog ai)
          ("https://simonwillison.net/tags/claude-code.atom"     blog ai programming)

          ;; ---------- Philosophy & Politics ----------
          ("https://chomsky.info/feed/"                          philosophy politics)
          ("https://www.radicalphilosophy.com/feed"              philosophy politics)
          ("https://nintil.com/rss.xml"                          philosophy science)
          ("http://www.unemployednegativity.com/feeds/posts/default" philosophy politics)
          ("https://monthlyreview.org/feed/"                     politics)
          ("https://stallman.org/rss/rss.xml"                    politics)
          ("https://cybershow.uk/rss/feed.xml"                   politics tech)

          ;; ---------- News, culture, misc ----------
          ("https://www.vox.com/rss/index.xml"                   news)
          ("https://daily.jstor.org/feed/"                       culture)
          ("http://feeds.feedburner.com/InformationIsBeautiful"  design data)
          ("https://www.admdnewsletter.com/rss/"                 marketing newsletter)
          ("https://www.leonardcohenforum.com/app.php/feed/topics" culture)

          ;; ---------- Arabic blogs ----------
          ("http://monakareem.blogspot.com/feeds/posts/default?alt=rss" blog arabic)
          ("https://rahmawritings.com/feed/"                     blog arabic)
          ("https://thematamixta.blogspot.com/feeds/posts/default" blog arabic)
          ("https://hawramani.com/feed/"                         blog arabic religion)
          ("https://melhamy.blogspot.com/feeds/posts/default"    blog arabic religion)
          ("https://etidal.org/feed/"                            arabic religion politics)
          ("https://mana.net/feed/"                              arabic culture)
          ("https://blog.tareef.sy/index.xml"                    blog arabic)

          ;; ---------- Aggregators — Hacker News ----------
          ("https://hnrss.org/frontpage"                         aggregator hn)
          ("https://hnrss.org/replies?id=lr0"                    aggregator hn personal)
          ("https://hnrss.org/polls"                             aggregator hn)
          ("https://hnrss.org/newest?q=emacs"                    aggregator hn emacs)
          ("https://hnrss.org/newest?q=chomsky"                  aggregator hn philosophy)
          ("https://hnrss.org/newest?q=israel"                   aggregator hn politics)
          ("https://hnrss.org/newest?q=egypt"                    aggregator hn arabic)
          ("https://hnrss.org/newest?q=arabic"                   aggregator hn arabic)
          ("https://hnrss.org/newest?q=arab"                     aggregator hn arabic)
          ("https://hnrss.org/newest?q=islam"                    aggregator hn religion)
          ("https://hnrss.org/newest?q=muslim"                   aggregator hn religion)

          ;; ---------- Aggregators — Lobsters ----------
          ("https://lobste.rs/rss"                               aggregator lobsters programming)
          ("https://lobste.rs/t/job.rss"                         aggregator lobsters jobs)

          ;; ---------- Aggregators — Reddit ----------
          ("https://www.reddit.com/r/emacs/.rss"                 aggregator reddit emacs)
          ("https://www.reddit.com/r/chomsky/search.rss?sort=new&restrict_sr=on&q=flair%3AArticle%2B"
           aggregator reddit philosophy politics)
          ("https://www.reddit.com/r/programmingcirclejerk/.rss" aggregator reddit humor)

          ;; ---------- Aggregators — StackExchange & misc ----------
          ("https://stackexchange.com/feeds/tagsets/451382/skepticism?sort=active"   aggregator stackexchange)
          ("https://stackexchange.com/feeds/tagsets/451144/lifehack?sort=active"     aggregator stackexchange)
          ("https://stackexchange.com/feeds/tagsets/450777/politics?sort=active"     aggregator stackexchange politics)
          ("https://stackexchange.com/feeds/tagsets/450949/interpersonal?sort=active" aggregator stackexchange)
          ("https://boards.4chan.org/sci/index.rss"              aggregator 4chan science)

          ;; ---------- Vienna RSS support (kept for transition reference) ----------
          ("https://github.com/ViennaRSS/vienna-rss/discussions.atom" meta)

          ;; ---------- YouTube — programming / CS ----------
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCX2U8kCH2EzKSeaTIJBtkkQ" youtube programming) ; ryan_cs
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" youtube programming) ; mentaloutlaw
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA" youtube programming) ; computerphile
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVpb6nv1igxnoY0SRVhaoEA" youtube programming) ; engieering
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsBjURrPoezykLs9EqgamOA" youtube programming) ; FireShip
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" youtube emacs)       ; systemcraft

          ;; ---------- YouTube — philosophy ----------
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCGVHC4L6gjS13AMe-JMOjHg" youtube philosophy)         ; kanb
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCmHu591mWNj_zSaSuYVwsaQ" youtube philosophy)         ; manufacturing_intellect
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkS_HP3m9NXOgswVAKbMeJQ" youtube philosophy)         ; thenandnow
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCdHT7KB1gDAXZYpPW71fn0Q" youtube philosophy)         ; distributist
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9ff15w4ufviWfv9UfIuByA" youtube philosophy history) ; wes_cecil
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC1VzCyqpmCaRh8_BnijbOvg" youtube philosophy)         ; Carneades

          ;; ---------- YouTube — history ----------
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCX7katl3DVmch4D7LSvqbVQ" youtube history) ; montemayor
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCEqNbbsx0i7fhwRt0saYIcQ" youtube history) ; america
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7pr_dQxm2Ns2KlzRSx5FZA" youtube history) ; sandrhoman
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCrI5U0R293u9uveijefKyAA" youtube history) ; ryan_history
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqBiWcuTF8IaLH7wBqnihsQ" youtube history) ; toldinstone
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCheAUBrk8xw6QhJIxEPsvhg" youtube history) ; thelifeguide
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMhLD2vecQSBqKd5uVPrwaQ" youtube history) ; historiancraft
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCp9ZtilfKJds0iWytR_pnOQ" youtube history) ; epimetheum
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCO6nDCimkF79NZRRb8YiDcA" youtube history) ; Storied

          ;; ---------- YouTube — math ----------
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCoxcjq-8xIDTYp3uz647V5A" youtube math) ; numberphile
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtYLUTtgS3k1Fg4y5tAhLbw" youtube math) ; statquest
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UChVUSXFzV8QCOKNWGfE56YQ" youtube math) ; BriTheMathGuy
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw" youtube math) ; 3Blue1Brown

          ;; ---------- YouTube — science ----------
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCHnyfMqiRRG1u-2MsSQLbXA" youtube science) ; veritasium
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q" youtube science) ; Kurzgesagt
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC1yNl2E66ZzKApQdRuTQ4tw" youtube science) ; Hossenfelder
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxoz4YfS4M3H3C57FD4jW4Q" youtube science) ; sciencetime
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCG9ShGbASoiwHwFcLcAh9EA" youtube science) ; sea
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCm3i_fqq8dqsV-dTAriv2KA" youtube science) ; disScience

          ;; ---------- YouTube — Arabic / religion ----------
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtx9oIT_eWp6jIkoULS-ZdQ" youtube arabic)          ; shabaka
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-pn1lcD--68WjOgCGBXlCQ" youtube arabic)          ; tasneem
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC_5ENzPnzYCGRxiOIdtaRug" youtube arabic art)      ; ramah aesthetic
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqBrZisk79ExL7MN96bEJxA" youtube arabic)          ; ayman
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCP-PfkMcOKriSxFMH7pTxfA" youtube arabic)          ; elhamy
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxPtfAOwtyd_N6keP3MnVmw" youtube arabic religion) ; Qassom
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9dRb4fbJQIbQ3KHJZF_z0g" youtube religion)        ; religion

          ;; ---------- YouTube — politics / news ----------
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UChzVhAwzGR7hV-4O8ZmBLHg" youtube politics) ; Glenn Greenwald

          ;; ---------- YouTube — art / aesthetic ----------
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC99wd4wi8AfaSkqprWo206g" youtube art) ; edits
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCrPOgNsUldOtQsTf9Kjlm_A" youtube art) ; national_gallery

          ;; ---------- YouTube — misc ----------
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtscFf8VayggrDYjOwDke_Q" youtube) ; academia
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCij1VEjDb88RCbqsvZ1gqaw" youtube) ; sideof
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCT6H50SccbeQKlhHufvAo1A" youtube) ; prof
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCHv1q35tKUJiSWO9W2OQRhw" youtube) ; age
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVHxGFLo5GzDNUHglYeW46Q" youtube)         ; english
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCL8w_A8p8P1HWI3k6PR5Z6w" youtube finance) ; two_cents
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9RM-iSvTu1uPJb8X5yp3EQ" youtube)         ; Wendover
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCJJl3hyuWwEw0EtYfzy0g5A" youtube)         ; workplace
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqM0zDcFNdAHj7uQkprLszg" youtube)         ; Positron
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCIBYr1IVf54Ho5fRV2s62QA" youtube)         ; alternate
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC8wKWWarusivFpIcUx9ilOw" youtube)         ; theDefault
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCiuDswZU50IGRKvF2AkVjrQ" youtube)         ; ACADEMY
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZlodzngfsaCQAKE6wXiuMw" youtube)         ; Fiction
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCePDFpCr78_qmVtpoB1Axaw" youtube)         ; Gart
          )))

;;; --- Background fetching: load elfeed and refresh feeds after startup

(defcustom salih/elfeed-update-interval (* 30 60)
  "Seconds between automatic background `elfeed-update-background' runs."
  :type 'integer
  :group 'elfeed)

(defvar salih/elfeed-update-timer nil
  "Repeating timer that drives background elfeed refreshes.")

(defun salih/elfeed-background-update ()
  "Fetch every feed in `elfeed-feeds' in the background.
`require's elfeed on the first call, which also loads the feed list and
DB settings from the `after! elfeed' block above.  Uses
`elfeed-update-background', which refreshes feeds without disturbing any
visible Elfeed windows and no-ops if an update is already running."
  (require 'elfeed)
  (elfeed-update-background))

(defun salih/elfeed-enable-background-updates ()
  "Do an initial background fetch and schedule periodic refreshes."
  (salih/elfeed-background-update)
  (unless (timerp salih/elfeed-update-timer)
    (setq salih/elfeed-update-timer
          (run-with-timer salih/elfeed-update-interval
                          salih/elfeed-update-interval
                          #'salih/elfeed-background-update))))

;; Once Doom and the user config have fully loaded, wait until Emacs is idle
;; briefly (so the first frame isn't blocked by loading elfeed + network I/O)
;; and then start fetching feeds in the background.  This means feeds are
;; already populated by the time `elfeed' is opened, instead of only fetching
;; on demand.
(add-hook 'doom-after-init-hook
          (lambda ()
            (unless noninteractive
              (run-with-idle-timer 5 nil #'salih/elfeed-enable-background-updates))))

(map! :leader
      :desc "Elfeed" "o e" #'elfeed)

(map! :after elfeed
      :map (elfeed-search-mode-map elfeed-show-mode-map)
      :n "C-c C-c" #'salih/elfeed-org-store-and-capture)



(defconst salih/elfeed-hn-item-url-regexp
  (concat "\\`" (regexp-quote "https://news.ycombinator.com/item?id=")
          "[0-9]+\\(?:[&#].*\\)?\\'")
  "Regexp matching Hacker News item URLs.")

(defconst salih/elfeed-hn-comments-url-regexp
  (concat "Comments URL:\\(?:.\\|\n\\)*?\\("
          (regexp-quote "https://news.ycombinator.com/item?id=")
          "[0-9]+\\(?:&amp;[[:alnum:]_=.-]+\\)*\\)")
  "Regexp matching the HNRSS comments URL in an entry body.")

(defun salih/elfeed--html-url-unescape (url)
  "Decode the small HTML entity subset that appears in feed URLs."
  (when url
    (replace-regexp-in-string "&amp;" "&" url t t)))

(defun salih/elfeed--hn-item-url-p (url)
  "Return non-nil when URL points at a Hacker News item page."
  (and (stringp url)
       (string-match-p salih/elfeed-hn-item-url-regexp
                       (salih/elfeed--html-url-unescape url))))

(defun salih/elfeed--entry-id-url (entry)
  "Return ENTRY's id string, when it is URL-shaped."
  (let ((id (and entry (elfeed-entry-id entry))))
    (cond
     ((stringp id) id)
     ((consp id) (cdr id)))))

(defun salih/elfeed-hn-comments-url (entry)
  "Return ENTRY's Hacker News comments URL, if HNRSS exposes one."
  (let ((content (and entry (elfeed-entry-content entry))))
    (or (when (and (stringp content)
                   (string-match salih/elfeed-hn-comments-url-regexp content))
          (salih/elfeed--html-url-unescape (match-string 1 content)))
        (let ((id-url (salih/elfeed--entry-id-url entry)))
          (when (salih/elfeed--hn-item-url-p id-url)
            (salih/elfeed--html-url-unescape id-url))))))

(defun salih/elfeed-entry-preferred-url (entry)
  "Return the URL `C' should open for ENTRY.
For HNRSS entries, prefer the Hacker News comments URL.  Otherwise fall back to
the article URL."
  (or (salih/elfeed-hn-comments-url entry)
      (and entry (elfeed-entry-link entry))))

(defun salih/elfeed--open-url-background (url)
  "Open URL in a browser, keeping focus in Emacs where supported."
  (cond
   ((eq system-type 'darwin)
    (start-process "elfeed-open-url-bg" nil "open" "-g" url))
   (t (start-process "elfeed-open-url" nil "xdg-open" url))))

(defun salih/elfeed-show-visit-feed ()
  "Open the preferred URL for the current Elfeed entry."
  (interactive)
  (let* ((entry elfeed-show-entry)
         (url (salih/elfeed-entry-preferred-url entry)))
    (unless url
      (user-error "No URL for this entry"))
    (salih/elfeed--open-url-background url)))



(defun salih/elfeed-visit-entry-background ()
  "Open the preferred URL for the current Elfeed entry."
  (interactive)
  (let* ((entry (or (bound-and-true-p elfeed-show-entry)
                    (elfeed-search-selected :single)))
         (url (salih/elfeed-entry-preferred-url entry)))
    (unless url
      (user-error "No URL for this entry"))
    (elfeed-search-untag-all-unread)
    (salih/elfeed--open-url-background url)))

(map! :after elfeed
      :map (elfeed-search-mode-map elfeed-show-mode-map)
      :nvim
      "C" #'salih/elfeed-visit-entry-background)

(map! :after elfeed
      :map elfeed-show-mode-map
      :nvim
      "C" #'salih/elfeed-show-visit-feed
      "C-n" #'elfeed-goodies/split-show-next
      "C-p" #'elfeed-goodies/split-show-prev)



(provide 'lr-elfeed)


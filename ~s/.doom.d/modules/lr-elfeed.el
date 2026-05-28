;;; lr-elfeed.el --- Elfeed RSS reader: feeds, tags, migration -*- lexical-binding: t; -*-

;;; --- Migration from Vienna: mark anything published before today as read.
;; The cutoff (start-of-today at first run) is persisted so subsequent
;; sessions, slow fetches, and backdated entries all stay marked.

(defvar salih/elfeed-cutoff-file
  (expand-file-name "elfeed-migration-cutoff" doom-cache-dir)
  "File storing the float-time cutoff. Entries dated before this are
auto-marked as read on insert.")

(defvar salih/elfeed-mark-old-cutoff nil
  "Cached epoch-seconds cutoff loaded from `salih/elfeed-cutoff-file'.")

(defun salih/elfeed--start-of-today ()
  (let ((d (decode-time)))
    (float-time (encode-time 0 0 0 (nth 3 d) (nth 4 d) (nth 5 d)))))

(defun salih/elfeed--load-cutoff ()
  "Read cutoff from disk, seeding to start-of-today on first run."
  (setq salih/elfeed-mark-old-cutoff
        (if (file-exists-p salih/elfeed-cutoff-file)
            (with-temp-buffer
              (insert-file-contents salih/elfeed-cutoff-file)
              (string-to-number (string-trim (buffer-string))))
          (let ((v (salih/elfeed--start-of-today)))
            (make-directory (file-name-directory salih/elfeed-cutoff-file) t)
            (with-temp-file salih/elfeed-cutoff-file
              (insert (number-to-string v)))
            v))))

(defun salih/elfeed-new-entry-mark-old (entry)
  "If ENTRY is dated before the migration cutoff, drop its unread tag."
  (when (and salih/elfeed-mark-old-cutoff
             (< (elfeed-entry-date entry) salih/elfeed-mark-old-cutoff))
    (setf (elfeed-entry-tags entry)
          (cl-remove 'unread (elfeed-entry-tags entry)))))

(defun salih/elfeed-mark-before-cutoff-read ()
  "Bulk pass: mark every DB entry dated before the cutoff as read.
Run this once after the first `elfeed-update' to clear the backlog."
  (interactive)
  (require 'elfeed)
  (unless salih/elfeed-mark-old-cutoff (salih/elfeed--load-cutoff))
  (let ((cutoff salih/elfeed-mark-old-cutoff)
        (n 0))
    (with-elfeed-db-visit (entry _feed)
      (when (and (< (elfeed-entry-date entry) cutoff)
                 (memq 'unread (elfeed-entry-tags entry)))
        (setf (elfeed-entry-tags entry)
              (cl-remove 'unread (elfeed-entry-tags entry)))
        (cl-incf n)))
    (elfeed-db-save)
    (when (derived-mode-p 'elfeed-search-mode)
      (elfeed-search-update--force))
    (message "elfeed: marked %d entries (dated before %s) as read"
             n (format-time-string "%Y-%m-%d %H:%M" cutoff))))

;;; --- Elfeed proper

(after! elfeed
  (setq elfeed-db-directory (expand-file-name "elfeed/" doom-cache-dir)
        elfeed-search-filter "@2-weeks-ago +unread"
        elfeed-search-title-max-width 100
        elfeed-search-title-min-width 30)

  (salih/elfeed--load-cutoff)
  (add-hook 'elfeed-new-entry-hook #'salih/elfeed-new-entry-mark-old)

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
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCCrl9a26fDCZvofnCnA5A8g" youtube)         ; Johnathan
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqM0zDcFNdAHj7uQkprLszg" youtube)         ; Positron
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCIBYr1IVf54Ho5fRV2s62QA" youtube)         ; alternate
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC8wKWWarusivFpIcUx9ilOw" youtube)         ; theDefault
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCiuDswZU50IGRKvF2AkVjrQ" youtube)         ; ACADEMY
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZlodzngfsaCQAKE6wXiuMw" youtube)         ; Fiction
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCePDFpCr78_qmVtpoB1Axaw" youtube)         ; Gart
          )))

(map! :leader
      :desc "Elfeed" "o e" #'elfeed)

(provide 'lr-elfeed)

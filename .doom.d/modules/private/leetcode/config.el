;;; mine/leetcode/config.el -*- lexical-binding: t; -*-


(setq leetcode-prefer-language "cpp")
(setq leetcode-prefer-sql "mssql")
(setq leetcode-save-solutions t)
(setq leetcode-directory "/home/ghd/gits/leet/ps/lc")

(map! :leader
      :desc "open leetcode"
      "l l" #'leetcode)

;;; ../configs/.doom.d/sqlconf.el -*- lexical-binding: t; -*-

;; (add-hook 'sql-mode-hook
;;         (lambda () (local-set-key (kbd "<f3>") #'ejc-connect-interactive)))
;; (add-hook 'sql-mode-hook
;;           (lambda () (local-set-key (kbd "<f2>") #'ejc-connect)))

;; sql

;;(require 'ejc-sql)
;;
;;(ejc-create-connection
;;"q"
;;:classpath "[/home/ghd/.m2/repository/postgresql/postgresql/9.3-1102.jdbc41/postgresql-9.3-1102.jdbc41.jar]"
;;:password ""
;;:user "postgres"
;;:port "5432"
;;:host "localhost"
;;:dbname "postgres"
;;:dbtype "postgresql")
;;
;;
;;(ejc-create-connection
;;"q"
;;:classpath "[/home/ghd/.m2/repository/postgresql/postgresql/9.3-1102.jdbc41/postgresql-9.3-1102.jdbc41.jar]"
;;:password ""
;;:user "postgres"
;;:port "5432"
;;:host "localhost"
;;:dbname "postgres"
;;:dbtype "postgresql")
;;
;;
;;
;;(ejc-create-connection
;;"ms"
;;:classpath "[/home/ghd/.m2/repository/com/microsoft/sqlserver/mssql-jdbc/6.2.2.jre8/mssql-jdbc-6.2.2.jre8.jar]"
;;:password "i7AvcLKSU4QpQr"
;;:user "sa"
;;:port "1433"
;;:host "localhost"
;;:dbname "ss"
;;:dbtype "sqlserver")
;;




;; (require 'ejc-autocomplete)
;; (add-hook 'ejc-sql-minor-mode-hook
;;          (lambda ()
;;            (auto-complete-mode t)
;;            (ejc-ac-setup)))
;; (require 'ejc-company)

;; (push 'ejc-company-backend company-backends)
;; (add-hook 'ejc-sql-minor-mode-hook
;;          (lambda ()
;;            (company-mode t)))
;; (setq ejc-complete-on-dot t)


;; (setq sql-connection-alist
;;      '((pool-a
;;         (sql-product 'postgresql)
;;         (sql-server "localhost")
;;         (sql-user "postgresql")
;;         (sql-password "")
;;         (sql-database "dvdrental ")
;;         (sql-port 5433))))






(use-package elfeed-dashboard
  :load-path "~/gits/lsp-mssql")
(require 'lsp-mssql)
(add-hook 'sql-mode-hook 'lsp)
(setq lsp-mssql-connections
      [(:server "localhost"
                :database "learning"
                :user "sa"
                :password "i7AvcLKSU4QpQr")])



(provide 'sqlconf)

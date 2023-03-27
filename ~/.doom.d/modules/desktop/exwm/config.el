;;; desktop/exwm/config.el -*- lexical-binding: t; -*-


(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
(require 'xcb)
(exwm-config-default)
(exwm-randr-enable)
(setq exwm-randr-workspace-monitor-plist '(0 "eDP-1"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command "xrandr --output HDMI-1-0 --auto --mode 1920x1080  --right-of eDP-1 ")))

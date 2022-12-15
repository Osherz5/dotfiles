(use-package exwm)
(require 'exwm-config)
(exwm-config-default)

(require 'exwm-randr)
(exwm-randr-enable)
(start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 2560x1440 --pos 0x0 --rotate normal")

;; These keys should always pass through to Emacs
(setq exwm-input-prefix-keys
      '(?\C-x
        ?\C-h
        ?\M-x
        ?\M-`
        ?\M-&
        ?\M-:
        ?\C-\M-j  ;; Buffer list
        ?\C-\M-k  ;; Browser list
        ?\C-\M-n  ;; Next workspace
        ?\C-\     ;; Ctrl+Space
        ?\C-\;))

;; Ctrl+Q will enable the next key to be sent directly
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;; Set up global key bindings.  These always work, no matter the input state!
;; Keep in mind that changing this list after EXWM initializes has no effect.
(setq exwm-input-global-keys
      `(
        ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
        ([?\s-r] . exwm-reset)

        ;; Move between windows
        ([s-left] . windmove-left)
        ([s-right] . windmove-right)
        ([s-up] . windmove-up)
        ([s-down] . windmove-down)

        ;; Launch applications via shell command
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))

        ;; Switch workspace
        ([?\s-w] . exwm-workspace-switch)


        ;; Cycle workspaces

        ;([?\s->] . exwm-workspace-current-index)
        ;([?\s-<] . exwm-workspace-switch)

        ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

        ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))

;; Simulation keys
(setq exwm-input-simulation-keys
      '(
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\M-d] . [S-end delete])
        ([?\C-k] . [S-end delete])
        ;([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ([?\C-/] . [?\C-z])
        ([?\C-g] . [?\C-c])))

;; Polybar
(start-process-shell-command "polybar" nil "polybar -r -c ~/.dotfiles/.polybar/config.ini mybar")

(exwm-enable)

(desktop-save-mode 0)
(setq-default frame-title-format '("%b"))

(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* (
         (backupRootDir "~/.emacs.d/backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
         )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
    )
  )

(defun mouse-start-rectangle (start-event)
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))

(global-set-key (kbd "S-<down-mouse-1>") #'mouse-start-rectangle)

;; xah-fly-keys settings
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)
;; (add-hook 'xah-fly-command-mode-activate-hook 'xah-fly-save-buffer-if-file)

(defun my-bindkey-xfk-insert-mode ()
  "Define keys for `xah-fly-insert-mode-activate-hook'"
  (interactive)
  (define-key xah-fly-key-map (kbd "ö") (lambda () 
	(interactive)
	(insert (char-from-name "LATIN SMALL LETTER O WITH DIAERESIS")))
    ;; more here
    ))

(defun my-bindkey-xfk-command-mode ()
  "Define keys for `xah-fly-command-mode-activate-hook'"
  (interactive)
  (define-key xah-fly-key-map (kbd "ö") 'xah-end-of-line-or-block)
  )

(add-hook 'xah-fly-insert-mode-activate-hook 'my-bindkey-xfk-insert-mode)
(add-hook 'xah-fly-command-mode-activate-hook 'my-bindkey-xfk-command-mode)

;; circe settings

(setq auth-sources '("~/.authinfo.gpg"))

(defun my-fetch-password (&rest params)
  (require 'auth-source)
  (let ((match (car (apply 'auth-source-search params))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "Password not found for %S" params))))

(defun my-nickserv-password (server)
  (my-fetch-password :login "dama_ " :machine "irc.freenode.net"))

(setq circe-network-options
      `(("Furnet"
         :host "eu.irc.furnet.org"
         :port "6697"
         :tls 't
         :nick "dama"
         :nickserv-mask "nickserv!services@irc.furnet.org"
         :nickserv-identify-challenge "This nickname is registered and protected."
         :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {password}"
         :nickserv-identify-confirmation "Password accepted - you are now recognized."
         :channels (:after-auth "#redditfurs" "#turri" "#tassu")
         :nickserv-password my-nickserv-password
         )
        ("Freenode"
         :host "chat.freenode.net"
         :port "6697"
         :tls 't
         :nick "sasha69"
         :nickserv-password my-nickserv-password
         :channels (:after-auth "##marxism")
         )
        ("Ponychat"
         :host "irc.eu.ponychat.net"
         :port "6697"
         :tls 't
         :nick "dama"
         :nickserv-mask "nickserv!nickserv@services.ponychat.net"
         :nickserv-identify-challenge "This nickname is registered."
         :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {nick} {password}"
         :nickserv-identify-confirmation "You are now identified for dama"
         :nickserv-password my-nickserv-password
         :channels (:after-auth "#brony.fi")
         )
        ("IRCnet"
         :host "irc.cc.tut.fi"
         :port "6667"
         :nick "dama"
         :channels ("#blanko" "#blanko.peli" "#blanko.2015")
         )
        ("Snoonet"
         :host "irc.snoonet.org"
         :port "6667"
         :nick "dama"
         )
        ("I2P"
         :host "localhost"
         :port "6668"
         :nick "dama"
         :channels (:after-auth "#leftsec")
         :nickserv-password my-nickserv-password
         :nickserv-identify-challenge "-NickServ- You have 30 seconds to identify to your nickname before it
    is changed."
         :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {password}"
         :nickserv-identify-confirmation "You are now identified for dama."
         :nickserv-mask "NickServ@services.irc.postman.i2p"
         )))

(autoload 'enable-circe-notifications "circe-notifications" nil t)
(add-hook 'circe-server-connected-hook 'enable-circe-notifications)

(defun my-irc-login ()
  "Login into my usual channels."
  (interactive)
  (eyebrowse-mode t)
  (make-frame-command)
  (next-multiframe-window)
  (split-window-below)
  (split-window-right)
  (other-window 2)
  (split-window-right)
  (switch-to-buffer "##marxism")
  (other-window 1)
  (switch-to-buffer "#blanko")
  (other-window 1)
  (switch-to-buffer "#blanko.peli")
  (other-window 1)
  (switch-to-buffer "#blanko.2015")
  (eyebrowse-switch-to-window-config-2)
  (switch-to-buffer "##traa")
  (eyebrowse-switch-to-window-config-1))

;; flyspell settings
(setq flyspell-correct-interface 'flyspell-correct-ivy)

(let ((langs '("british" "finnish")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))
(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(global-set-key (kbd "<f5>") 'cycle-ispell-languages)

(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-ä") 'flyspell-correct-previous-word-generic))

;; packages
(setq flycheck-global-modes '(not eclim-mode))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(load "dired+")
(diredp-toggle-find-file-reuse-dir 1)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")

(setq eclimd-autostart t)

(defun my-java-mode-hook ()
  (eclim-mode t))

(add-hook 'java-mode-hook 'my-java-mode-hook)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

;; mu4e settings
(autoload 'mu4e "mu4e" "Launch mu4e and show the main window" t)
;;(load "mu4e")

(setq mu4e-maildir "~/Maildir")
(setq mu4e-get-mail-command "/usr/bin/mbsync -a"
      mu4e-update-interval 300)
(setq mu4e-sent-folder "/disroot/Sent")
(setq mu4e-trash-folder "/disroot/Trash")
(setq mu4e-drafts-folder "/disroot/Drafts")
(setq mu4e-refile-folder "/disroot/Archive")

(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)
(setq shr-color-visible-luminance-min 80)
(setq shr-color-visible-distance-min 5)

(mu4e-maildirs-extension)
(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

(setq mu4e-change-filenames-when-moving t)

(setq mu4e-maildir-shortcuts
      '( ("/disroot/INBOX"     . ?i)
         ("/disroot/Sent" . ?s)
         ("/disroot/Trash" . ?t)))

(setq user-mail-address "sashaa@disroot.org"
      user-full-name "Sasha Abbott"
      )

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "disroot.org"
      smtpmail-stream-type  'starttls
      smtpmail-smtp-service 587)

(fset 'my-move-to-trash "mt")
(define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
(define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)

(provide 'init-local)

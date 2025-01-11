;;; fabio emacs config 

(setq inhibit-splash-screen t)

(custom-set-faces
 '(fringe ((t (:background "black")))))

(defun rz-margins ()
  (when (eq (current-buffer) (get-buffer "*scratch*"))
  (let ((margin-size (/ (- (frame-width) 80) 2)))
    (set-window-margins nil margin-size margin-size))))

(defun s-buffer ()
  (interactive)
  (save-buffer))

(defun mk-shell-below ()
  (interactive)
  (if (get-buffer-window "*shell*")
      (message "shell is already open")
    (let ((w (split-window-below)))
      (select-window w)
      (shell))))

(defun c-startup-screen ()
  (with-current-buffer (get-buffer-create "*scratch*")
    (erase-buffer)
    (insert "\n\n")
    (insert
     "
                  ██╗  ██╗███████╗██╗     ██╗      ██████╗ 
                  ██║  ██║██╔════╝██║     ██║     ██╔═══██╗
                  ███████║█████╗  ██║     ██║     ██║   ██║
                  ██╔══██║██╔══╝  ██║     ██║     ██║   ██║
                  ██║  ██║███████╗███████╗███████╗╚██████╔╝
                  ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝ ╚═════╝ 
\n")
    (insert "\n\n")
    (goto-char (point-min))))

(keymap-global-set "C-e" 'shell-command)
(global-set-key (kbd "C-s") 's-buffer)
(global-set-key (kbd "C-M-e") 'mk-shell-below)

(add-hook 'after-init-hook 'c-startup-screen)
(add-hook 'after-init-hook #'rz-margins)
(add-hook 'window-configuration-change-hook #'rz-margins)

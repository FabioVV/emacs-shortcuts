;;; fabio emacs config 

(setq inhibit-splash-screen t)
(setq ring-bell-function 'ignore)
(desktop-save-mode 1) 

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
	  	  
(defun my-homepage-button-action (&rest _)
  (interactive)
  (browse-url "https://www.example.com"))
  
(defun dk-read (button)
  (desktop-read))  
  
(defface my-button-face
  '((t (:background nil :foreground "white" :box nil)))
  "Custom face for my button.")
		  

(defun create-centered-button (label action)
  "Create a centered button with LABEL and ACTION."
  (let* ((line-width (window-width))  ;; Get the width of the current window
         (button-width (length label))  ;; Get the width of the button label
         (spaces (max 0 (/ (- line-width button-width) 2))))  ;; Calculate leading spaces
    (insert (make-string spaces ?\s))  ;; Insert leading spaces
    (insert-text-button label
                        'action action
                        'help-echo (format "Click to %s" label)
                        'face 'my-button-face)
    (insert "\n\n")))  ;; Add two newlines for spacing  


(defun open-file-manager (button)
  "Open a graphical directory chooser dialog and open the selected directory in dired."
  (interactive)
  (let ((directory (read-directory-name "Choose a directory: ")))
    (dired directory)))  ;; Open the selected directory in dired
	
(defun open-emacs-file (button)
  "Open the .emacs configuration file."
  (interactive)
  (find-file "~/.emacs"))  ;; Adjust the path if your config file is located elsewhere

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
	
    (insert "================================================================================")
    (insert "\n\n")

	(create-centered-button "--- Reload last session ---" 'dk-read)
	(create-centered-button "--- Open project ---" 'open-file-manager)
	(create-centered-button "--- Open config ---" 'open-emacs-file)
	(create-centered-button "--- Get to know me ---" 'my-homepage-button-action)
	

	(insert "================================================================================")

	
	(center-region (line-beginning-position) (line-end-position))
	(insert "\n This is a simplistic configuration for emacs based on my taste\n")

    (goto-char (point-min))
	(read-only-mode 1)))




(keymap-global-set "C-e" 'shell-command)
(global-set-key (kbd "C-s") 's-buffer)
(global-set-key (kbd "C-M-e") 'mk-shell-below)

(add-hook 'after-init-hook 'c-startup-screen)
(add-hook 'after-init-hook #'rz-margins)
(add-hook 'window-configuration-change-hook #'rz-margins)

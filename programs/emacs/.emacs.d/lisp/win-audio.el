;; Depends on nircmd
;; https://www.nirsoft.net/utils/nircmd.html

;; Use speakers
(defun win-audio-speakers ()
  (interactive)
  (shell-command "nircmd setdefaultsounddevice \"Speakers\" 1"))

;; Use headset
(defun win-audio-headset ()
  (interactive)
  (shell-command "nircmd setdefaultsounddevice \"Headset Earphone\" 1"))

;; Use TV
(defun win-audio-tv ()
  (interactive)
  (shell-command "nircmd setdefaultsounddevice \"4 - DELL U2913WM\" 1"))

(provide 'win-audio)

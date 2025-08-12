;;; completing-linux-apps.el --- completing-read interface to run programs with xdg desktop entries  -*- lexical-binding:t -*-

;; Copyright (C) 2025, Karl Ragnar Giese

;; Author: Karl Ragnar Giese <Karl@Giese.no>
;; Version: 1.0

(defvar completing-linux-apps--desktop-entry-cache nil)


(defun completing-linux-apps--desktop-entry-files ()
  "Return list of desktop entry files

These are found by recursively searching xdg data dirs for files ending in .desktop"
  (mapcan
   (lambda (f) (ignore-errors (directory-files-recursively f ".*\.desktop$")))
   (xdg-data-dirs)))


(defun completing-linux-apps--build-cache ()
  "Build the cache, consisting of (name . exec) cons cells"
  (setq completing-linux-apps--desktop-entry-cache 
	(let* ((files (completing-linux-apps--desktop-entry-files))
	       (entries (mapcar (lambda (file) (xdg-desktop-read-file file)) files)))
	  (mapcar (lambda (entry) (cons (gethash "Name" entry)
				   (gethash "Exec" entry)))
		  entries))))


(defun completing-linux-apps-clear-cache ()
  "Clear cache"
  (interactive)
  (setq completing-linux-apps--desktop-entry-cache nil))


(defun completing-linux-apps-run-program ()
  "Run a program with an xdg desktop entry. If no matches are found, run as an async-shell-command instead."
  (interactive)
  (unless completing-linux-apps--desktop-entry-cache
    (completing-linux-apps--build-cache))
  (let* ((name (completing-read "Run: " completing-linux-apps--desktop-entry-cache))
	 (exec (or (cdr (assoc name completing-linux-apps--desktop-entry-cache))
		   name)))
    (async-shell-command exec)))


(provide 'completing-linux-apps)

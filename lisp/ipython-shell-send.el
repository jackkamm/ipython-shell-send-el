(require 'python)

(defun ipython-shell--save-temp-file (string)
  (let* ((temporary-file-directory
          (if (file-remote-p default-directory)
              (concat (file-remote-p default-directory) "/tmp")
            temporary-file-directory))
         (temp-file-name (make-temp-file "ipy" nil ".ipy"))
         (coding-system-for-write (python-info-encoding)))
    (with-temp-file temp-file-name
      (insert string)
      (delete-trailing-whitespace))
    temp-file-name))

(defun ipython-shell-send-string (string &optional process msg)
  "Send STRING to inferior Python PROCESS.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list (read-string "Python command: ") nil t))
  (let ((process (or process (python-shell-get-process-or-error msg))))
    (if (string-match ".\n+." string)   ;Multiline.
        (let* ((temp-file-name (ipython-shell--save-temp-file string))
               (file-name (or (buffer-file-name) temp-file-name)))
          (message temp-file-name)
          (ipython-shell-send-file file-name process temp-file-name t))
      (comint-send-string process string)
      (when (or (not (string-match "\n\\'" string))
                (string-match "\n[ \t].*\n?\\'" string))
        (comint-send-string process "\n")))))

(defun ipython-shell-send-region (start end &optional send-main msg)
  "Send the region delimited by START and END to inferior Python process.
When optional argument SEND-MAIN is non-nil, allow execution of
code inside blocks delimited by \"if __name__== \\='__main__\\=':\".
When called interactively SEND-MAIN defaults to nil, unless it's
called with prefix argument.  When optional argument MSG is
non-nil, forces display of a user-friendly message if there's no
process running; defaults to t when called interactively."
  (interactive
   (list (region-beginning) (region-end) current-prefix-arg t))
  (let* ((string (python-shell-buffer-substring start end (not send-main)))
         (process (python-shell-get-process-or-error msg))
         (original-string (buffer-substring-no-properties start end))
         (_ (string-match "\\`\n*\\(.*\\)" original-string)))
    (message "Sent: %s..." (match-string 1 original-string))
    (ipython-shell-send-string string process)))

(defun ipython-shell-send-buffer (&optional send-main msg)
  "Send the entire buffer to inferior Python process.
When optional argument SEND-MAIN is non-nil, allow execution of
code inside blocks delimited by \"if __name__== \\='__main__\\=':\".
When called interactively SEND-MAIN defaults to nil, unless it's
called with prefix argument.  When optional argument MSG is
non-nil, forces display of a user-friendly message if there's no
process running; defaults to t when called interactively."
  (interactive (list current-prefix-arg t))
  (save-restriction
    (widen)
    (ipython-shell-send-region (point-min) (point-max) send-main msg)))

(defun ipython-shell-send-defun (&optional arg msg)
  "Send the current defun to inferior Python process.
When argument ARG is non-nil do not include decorators.  When
optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive (list current-prefix-arg t))
  (save-excursion
    (ipython-shell-send-region
     (progn
       (end-of-line 1)
       (while (and (or (python-nav-beginning-of-defun)
                       (beginning-of-line 1))
                   (> (current-indentation) 0)))
       (when (not arg)
         (while (and (forward-line -1)
                     (looking-at (python-rx decorator))))
         (forward-line 1))
       (point-marker))
     (progn
       (or (python-nav-end-of-defun)
           (end-of-line 1))
       (point-marker))
     nil  ;; noop
     msg)))

(defun ipython-shell-send-file (file-name &optional process temp-file-name
                                         delete msg)
  "Send FILE-NAME to inferior Python PROCESS.
If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME.  If TEMP-FILE-NAME and DELETE are non-nil, then
TEMP-FILE-NAME is deleted after evaluation is performed.  When
optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list
    (read-file-name "File to send: ")   ; file-name
    nil                                 ; process
    nil                                 ; temp-file-name
    nil                                 ; delete
    t))                                 ; msg
  (let* ((process (or process (python-shell-get-process-or-error msg)))
         (file-name (expand-file-name
                     (or (file-remote-p file-name 'localname)
                         file-name)))
         (temp-file-name (when temp-file-name
                           (expand-file-name
                            (or (file-remote-p temp-file-name 'localname)
                                temp-file-name)))))
    (python-shell-send-string
     (format
      (concat
       "import IPython, os;"
       "IPython.get_ipython().magic('''run -i %s''');"
       (when (and delete temp-file-name)
         (format "os.remove('''%s''');" temp-file-name)))
      (or temp-file-name file-name))
     process)))

(provide 'ipython-shell-send)

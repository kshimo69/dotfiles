;; lingr.el --- Lingr Client for GNU Emacs

;; Copyright (C) 2010 lugecy <lugecy@gmail.com>

;; Author: lugecy <lugecy@gmail.com>
;; URL: http://github.com/lugecy/lingr-el
;; Keywords: chat, client, Internet
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary
;; Lingr (web chat service: http://lingr.com/) client for GNU Emacs

;;; Usage:
;; (require 'lingr)
;; (setq lingr-username <username>
;;       lingr-password <password>)
;; and
;; M-x lingr-login

;;; Code:

(eval-when-compile (require 'cl))
(require 'json)
(require 'url)
(require 'parse-time)
(require 'timezone)

;;;; Internal variables
(defgroup lingr nil
  "Lingr mode."
  :group 'chat
  :prefix "lingr-")

(defcustom lingr-username nil
  "Lingr username."
  :type 'string
  :group 'lingr)

(defcustom lingr-password nil
  "Lingr password."
  :type 'string
  :group 'lingr)

(defcustom lingr-url-show-status nil
  "If non-nil, show read status of url package."
  :type 'boolean
  :group 'lingr)

(defcustom lingr-show-update-notification t
  "If non-nil, show update summay."
  :type 'boolean
  :group 'lingr)

(defvar lingr-room-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'lingr-say-command)
    (define-key map (kbd "C-c C-b") 'lingr-switch-room)
    (define-key map (kbd "u") 'lingr-say-command)
    (define-key map (kbd "r") 'lingr-switch-room)
    map)
  "Lingr room mode map.")

(defvar lingr-base-url "http://lingr.com/api/")
(defvar lingr-observe-base-url "http://lingr.com:8080/api/")
(defvar lingr-http-response-json nil)
(defvar lingr-session-data nil)
(defvar lingr-logout-session-flg nil)
(defvar lingr-subscribe-counter nil)
(defvar lingr-buffer-basename "Lingr")
(defvar lingr-buffer-room-id nil)
(defvar lingr-room-list nil)
(defvar lingr-say-winconf nil)
(defvar lingr-say-window-height-per 20)
(defvar lingr-say-buffer "*Lingr Say*")

;;;; Utility Macro
(defmacro lingr-aif (test-form then-form &rest else-forms)
  (declare (indent 2)
           (debug (form form &rest form)))
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

;;;; http access utility
(defun lingr-http-get (path args &optional callback async cbargs)
  "Send ARGS to PATH as a GET request."
  (lingr-http-session "GET" (concat (if (string-equal path "event/observe")
                                        lingr-observe-base-url
                                      lingr-base-url)
                                    path)
                      args callback async cbargs))

(defun lingr-http-post (path args &optional callback async cbargs)
  "Send ARGS to PATH as a POST request."
  (lingr-http-session "POST" (concat lingr-base-url path)
                      args callback async cbargs))

(defun lingr-http-session (method url args &optional callback async cbargs)
  (let* ((data-string (mapconcat (lambda (arg)
                                   (concat (url-hexify-string (car arg))
                                           "="
                                           (url-hexify-string (cdr arg))))
                                 args
                                 "&"))
         (response-callback (or callback 'lingr-default-callback))
         (request-url (if (string-equal method "GET")
                          (concat url "?" data-string)
                        url))
         (url-request-method method)
         (url-request-data (if (string-equal method "GET")
                               ""
                             data-string))
         (url-show-status lingr-url-show-status))
    (if async
        (let ((buffer (url-retrieve request-url 'lingr-api-access-callback (append (list response-callback) cbargs))))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (set (make-local-variable 'url-show-status)
                   lingr-url-show-status)))
          buffer)
      (lingr-aif (url-retrieve-synchronously request-url)
          (with-current-buffer it
            (lingr-api-access-callback nil response-callback))))))

(defun lingr-api-access-callback (status func &rest args)
  (when (eq (car status) :error)
    (error "GET/POST fail."))
  (let ((json (lingr-get-json-data))
        (buffer (current-buffer)))
    (unless (equal (assoc-default 'status json) "ok")
      (error "Lingr API Error: %s" json))
    (kill-buffer buffer)
    (apply func (cons json args))))

;;;; Data struct access functions
(defun lingr-session-id (session) (assoc-default 'session session))
(defun lingr-session-nick (session) (assoc-default 'nickname session))

(defun lingr-response-counter (json) (assoc-default 'counter json))
(defun lingr-response-events (json) (assoc-default 'events json))
(defun lingr-response-rooms (json) (assoc-default 'rooms json))

(defun lingr-roominfo-id (roominfo) (assoc-default 'id roominfo))
(defun lingr-roominfo-messages (roominfo) (assoc-default 'messages roominfo))

(defun lingr-room-id (room) (car room))
(defun lingr-room-buffer (room) (cadr room))
(defun lingr-get-room-buffer (room-id) (lingr-room-buffer (assoc room-id lingr-room-list)))

(defun lingr-event-message (event) (assoc-default 'message event))
(defun lingr-event-presence (event) (assoc-default 'presence event))

(defun lingr-message-nick (message) (assoc-default 'nickname message))
(defun lingr-message-text (message) (assoc-default 'text message))
(defun lingr-message-room (message) (assoc-default 'room message))
(defun lingr-message-timestamp (message) (assoc-default 'timestamp message))

(defun lingr-presence-text (presence) (assoc-default 'text presence))
(defun lingr-presence-room (presence) (assoc-default 'room presence))
(defun lingr-presence-timestamp (presence) (assoc-default 'timestamp presence))

;;;; Lingr API functions
(defun lingr-api-session-create (user password)
  (lingr-http-post "session/create"
                   `(("user" . ,user) ("password" . ,password))
                   (lambda (json &rest args) (setq lingr-session-data json))))

(defun lingr-api-session-verify (session-id)
  (lingr-http-get "session/verify"
                  `(("session" . ,session-id))))

(defun lingr-api-session-destroy (session)
  (lingr-aif (lingr-session-id session)
      (lingr-http-post "session/destroy" `(("session" . ,it)))))

(defun lingr-api-set-presence (session presence)
  (lingr-aif (lingr-session-id session)
      (lingr-http-post "session/set_presence"
                       `(("session" . ,it) ("presence" . ,presence)
                         ("nickname" . ,(lingr-session-nick session))))))

(defun lingr-api-get-rooms (session)
  (lingr-aif (lingr-session-id session)
      (lingr-http-get "user/get_rooms" `(("session" . ,it))
                      (lambda (json &rest args) (lingr-response-rooms json)))))

(defun lingr-api-room-show (session room)
  (lingr-aif (lingr-session-id session)
      (lingr-http-get "room/show"
                      `(("session" . ,it) ("room" . ,room))
                      'lingr-api-room-show-callback t (list it))))

(defun lingr-api-get-archives (session room max_message_id)
  (let ((limit "100"))
    (lingr-aif (lingr-session-id session)
        (lingr-http-get "room/get_archives"
                        `(("session" . ,it) ("room" . ,room)
                          ("before" . ,max_message_id) ("limit" . ,limit))
                        nil t))))

(defun lingr-api-subscribe (session room &optional reset)
  (lingr-aif (lingr-session-id session)
      (lingr-http-post "room/subscribe"
                       `(("session" . ,it) ("room" . ,room)
                         ("reset" . ,(or reset "true")))
                       'lingr-api-subscribe-callback)))

(defun lingr-api-unsubscribe (session room)
  (lingr-aif (lingr-session-id session)
      (lingr-http-post "room/unsubscribe"
                       `(("session" . ,it) ("room" . ,room)))))

(defun lingr-api-say (session room text)
  (lingr-aif (lingr-session-id session)
      (lingr-http-post "room/say"
                       `(("session" . ,it) ("room" . ,room)
                         ("nickname" . ,(lingr-session-nick session))
                         ("text" . ,(encode-coding-string text 'utf-8)))
                       nil t)))

(defun lingr-api-observe (session)
  (lingr-aif (lingr-session-id session)
      (when lingr-subscribe-counter
        (lingr-http-get "event/observe"
                        `(("session" . ,it) ("counter" . ,(number-to-string lingr-subscribe-counter)))
                        'lingr-api-observe-callback t (list it lingr-subscribe-counter)))))

;;;; Lingr API callback functions
(defun lingr-default-callback (json &rest args)
  (setq lingr-http-response-json json))

(defun lingr-api-room-show-callback (json &rest args)
  (setq lingr-http-response-json json)
  (when (and lingr-session-data
             (string-equal (lingr-session-id lingr-session-data) (car-safe args)))
    (lingr-refresh-rooms json)
    (switch-to-buffer (lingr-room-buffer (car lingr-room-list)))))

(defun lingr-api-subscribe-callback (json &rest args)
  (setq lingr-http-response-json json)
  (setq lingr-subscribe-counter (lingr-response-counter json)))

(defun lingr-api-observe-callback (json &rest args)
  (setq lingr-http-response-json json)
  (lingr-debug-observe-log json)
  (when (and lingr-session-data
             (string-equal (lingr-session-id lingr-session-data) (car args))
             (eq lingr-subscribe-counter (cadr args)))
    (lingr-aif (lingr-response-counter json)
        (setq lingr-subscribe-counter it))
    (lingr-aif (lingr-response-events json)
        (let ((updates (loop for event across it
                             collect (lingr-update-by-event event))))
          (when lingr-show-update-notification
            (lingr-show-update-summay updates))))
    (unless lingr-logout-session-flg
      (lingr-api-observe lingr-session-data))))

;;;; Utility function
(defmacro lingr-update-with-buffer (buffer &rest body)
  (declare (indent 1))
  `(when (buffer-live-p ,buffer)
     (with-current-buffer ,buffer
       (goto-char (point-max))
       (setq buffer-read-only nil)
       ,@body
       (setq buffer-read-only t))))

(defun lingr-get-json-data ()
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)
      (json-read-from-string (buffer-substring-no-properties (point) (point-max))))))

(defun lingr-event-type (event)
  (cond ((lingr-event-message event) 'message)
        ((lingr-event-presence event) 'presence)
        (t nil)))

(defun lingr-decode-message-text (message)
  (let ((text-cons (assoc 'text message))
        (nick-cons (assoc 'nickname message)))
    (setcdr text-cons (decode-coding-string (cdr text-cons) 'utf-8))
    (setcdr nick-cons (decode-coding-string (cdr nick-cons) 'utf-8))
    message))

(defun lingr-decode-timestamp (timestamp)
  (format-time-string "[%x %T]" (apply 'encode-time (parse-time-string (timezone-make-date-arpa-standard timestamp)))))

(defun lingr-insert-message (message)
  (let* ((nick (lingr-message-nick message))
         (nick-str (concat nick (make-string (max (- 12 (string-width nick)) 0) ? )))
         (text (lingr-message-text message))
         (time-str (lingr-decode-timestamp (lingr-message-timestamp message))))
    (insert (format "%s%s: %s\n"
                    time-str nick-str
                    (replace-regexp-in-string "\n" (concat "\n" (make-string (+ (string-width (concat time-str nick-str)) 2) ? )) text)))))

(defun lingr-refresh-rooms (json)
  (setq lingr-room-list
        (loop for roominfo across (lingr-response-rooms json)
              with room-list
              do
              (with-current-buffer (get-buffer-create (format "%s[%s]"
                                                              lingr-buffer-basename
                                                              (lingr-roominfo-id roominfo)))
                (lingr-room-mode)
                (setq buffer-read-only nil)
                (erase-buffer)
                (setq lingr-buffer-room-id (lingr-roominfo-id roominfo))
                (goto-char (point-min))
                (loop for message across (lingr-roominfo-messages roominfo)
                      do
                      (lingr-decode-message-text message)
                      (lingr-insert-message message))
                (setq buffer-read-only t)
                (push (list (lingr-roominfo-id roominfo) (current-buffer)) room-list))
              finally return (reverse room-list))))

(defun lingr-update-by-event (event)
  (case (lingr-event-type event)
    (message
     (let ((message (lingr-event-message event)))
       (lingr-decode-message-text message)
       (lingr-update-with-buffer (lingr-get-room-buffer (lingr-message-room message))
         (lingr-insert-message message))
       (list 'message (lingr-message-room message))))
    (presence
     (let ((presence (lingr-event-presence event)))
       (lingr-update-with-buffer (lingr-get-room-buffer (lingr-presence-room presence))
         (let ((timestamp (lingr-presence-timestamp presence))
               (text (lingr-presence-text presence)))
           (insert (format "%s%s\n" (lingr-decode-timestamp timestamp) text))))
       (list 'presence (lingr-presence-room presence))))
    (t nil)))

(defun lingr-show-update-summay (updates)
  (lingr-aif (delete-dups (loop for (type room) in updates
                                if (eq type 'message) collect room))
      (message "Lingr update message in %s." (mapconcat 'identity it ","))))

(defun lingr-presence-online ()
  (lingr-api-set-presence lingr-session-data "online"))

(defun lingr-presence-offline ()
  (lingr-api-set-presence lingr-session-data "offline"))

(defun lingr-room-mode ()
  "Major mode for Lingr.
Special commands:

\\{lingr-room-map}"
  (kill-all-local-variables)
  (setq major-mode 'lingr-room-mode
        mode-name "Lingr-Room")
  (make-local-variable 'lingr-buffer-room-id)
  (use-local-map lingr-room-map))

;;;; Interactive functions
(defun lingr-login (&optional username password)
  (interactive (list (or lingr-username (read-from-minibuffer "Username: "))
                     (or lingr-password (read-passwd "Password: "))))
  (unless username (setq username lingr-username))
  (unless password (setq password lingr-password))
  (unless (and username password)
    (error "Empty username or password."))
  (lingr-aif (lingr-api-session-create username password)
      (let* ((rooms (lingr-api-get-rooms it))
             (rooms-query (mapconcat 'identity rooms ",")))
        (when rooms
          (lingr-api-subscribe it rooms-query)
          (lingr-api-room-show it rooms-query)
          (lingr-api-observe it)))))

(defun lingr-logout ()
  (interactive)
  (when lingr-session-data
    (setq lingr-logout-session-flg t)
    (let* ((rooms (lingr-api-get-rooms lingr-session-data))
           (rooms-query (mapconcat 'identity rooms ",")))
      (when rooms
        (lingr-api-unsubscribe lingr-session-data rooms-query)))
    (lingr-api-session-destroy lingr-session-data)
    (setq lingr-subscribe-counter nil
          lingr-session-data nil
          lingr-logout-session-flg nil)))

(defun lingr-say-command ()
  (interactive)
  (unless lingr-buffer-room-id
    (error "This is not Lingr Chat buffer."))
  (setq lingr-say-winconf (current-window-configuration))
  (let ((room-id lingr-buffer-room-id))
    (condition-case nil
        (progn
          (split-window (selected-window)
                        (max (round (* (window-height)
                                       (/ (- 100 lingr-say-window-height-per) 100.0)))
                             5))
          (other-window 1)
          (switch-to-buffer (get-buffer-create lingr-say-buffer))
          (make-local-variable 'lingr-buffer-room-id)
          (setq lingr-buffer-room-id room-id)
          (local-set-key (kbd "C-c C-c") 'lingr-say-execute)
          (local-set-key (kbd "C-c C-k") 'lingr-say-abort))
      (error (call-interactively 'lingr-say-command-internal)))))

(defun lingr-say-command-internal (text)
  (interactive "sLingr-Say: ")
  (if lingr-buffer-room-id
      (lingr-api-say lingr-session-data lingr-buffer-room-id text)
    (error "This is not Lingr Chat buffer.")))

(defun lingr-say-execute ()
  (interactive)
  (when (> (length (buffer-string)) 0)
    (lingr-say-command-internal (buffer-string)))
  (kill-buffer (current-buffer))
  (when lingr-say-winconf
    (set-window-configuration lingr-say-winconf)))

(defun lingr-say-abort ()
  (interactive)
  (kill-buffer (current-buffer))
  (when lingr-say-winconf
    (set-window-configuration lingr-say-winconf)))

(defun lingr-switch-room (room-id)
  (interactive (list (completing-read "Switch Room: "
                                      (mapcar (lambda (room) (lingr-room-id room))
                                              lingr-room-list)
                                      nil t)))
  (switch-to-buffer (lingr-get-room-buffer room-id)))

(defun lingr-refresh-all-room ()
  (interactive)
  (lingr-aif (mapcar (lambda (room) (car room)) lingr-room-list)
      (lingr-api-room-show lingr-session-data (mapconcat 'identity it ","))))

;;;; Debug Utility
(defvar lingr-debug nil)
(defvar lingr-debug-log-buffer "*Lingr Debug Log*")
(defun lingr-debug-observe-log (obj)
  (when lingr-debug
    (with-current-buffer (get-buffer-create lingr-debug-log-buffer)
      (goto-char (point-max))
      (ignore-errors
        (insert (format "%s | %s\n" (format-time-string "%x %T") (prin1-to-string obj)))
        (goto-char (point-max))))))

(provide 'lingr)

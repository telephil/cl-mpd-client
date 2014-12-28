;; This file is part of mpd-client.
;;
;; mpd-client is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; mpd-client is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with mpd-client.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:mpd-client)

;;;;;;;;;;;;;;;;;;;
;;;; CONSTANTS ;;;;
;;;;;;;;;;;;;;;;;;;
(defconstant +default-hostname+ "localhost")
(defconstant +default-port+ 6600)

(defconstant +error-re+ "ACK\\s(.+)")
(defconstant +version-re+ "OK MPD\\s(.+)")
(defconstant +pair-re+ "\\w+:\\s(.+)")
(defconstant +key-value-pair-re+ "(.+):\\s(.+)")

;;;;;;;;;;;;;;;
;;;; UTILS ;;;;
;;;;;;;;;;;;;;;  
(defun safe-parse-integer (str &optional (default 0))
  (or (parse-integer str :junk-allowed t) default))

(defun string-match (str what)
  (equal (string-downcase str) what))

(defun regexp-match (regexp line)
  "Scan line for regexp and return the first result if found"
  (multiple-value-bind (all match)
      (cl-ppcre:scan-to-strings regexp line)
    (declare (ignore all))
    (when match
      (aref match 0))))

(defun get-pair-value (line)
  "Retrieve the value from a key/value pair in the form 'key: value'"
  (regexp-match +pair-re+ line))

(defun get-version (line)
  "Retrieve the version from the MPD status line"
  (regexp-match +version-re+ line))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RESULT HANDLERS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun default-result-handler (result)
  "This is the default function used to process response from MPD server.
   Simply returns true"
  (declare (ignore result))
  t)

(defun identity-result-handler (result)
  "This handler return the result as is
   This is mostly used for testing purposes."
  result)

(defun pair-result-handler (result)
  "This handler treat each element of the response list as a pair 'key: value'.
   Removes the key and return a list with the values"
  (mapcar #'get-pair-value result))

(defun song-info-result-handler (result)
  "This handler returns a list of song-info instances"
  (let ((current nil)
        (res '()))
    (dolist (l result)
      (multiple-value-bind (str match) (scan-to-strings +key-value-pair-re+ l)
        (declare (ignore str))
        (when match
          (let ((k (aref match 0))
                (v (aref match 1)))
            (when (equal k "file")
              (when (not (null current))
                (push current res))
              (setf current (make-instance 'mpd-song)))
            (cond
              ((string-match k "id")
	       (setf (song-id current) (safe-parse-integer v)))
              ((string-match k "pos")
	       (setf (song-position current) (safe-parse-integer v)))
              ((string-match k "file")
	       (setf (song-file current) v))
              ((string-match k "title")
	       (setf (song-title current) v))
              ((string-match k "album")
	       (setf (song-album current) v))
              ((string-match k "artist")
	       (setf (song-artist current) v))
              ((string-match k "time") 
	       (setf (song-time current) (safe-parse-integer v)))
              ((string-match k "track")
	       (setf (song-track current) (safe-parse-integer v)))
              ((string-match k "genre")
	       (setf (song-genre current) v))
              (t
	       (format t "Unknow key '~a' with value '~a'~%" k v)))))))
    (push current res)
    (reverse res)))

(defun stats-result-handler (result)
  "This handler returns an instance of the mpd-stats class"
  (let ((stats (make-instance 'mpd-stats)))
    (dolist (l result)
      (multiple-value-bind (str match) (scan-to-strings +key-value-pair-re+ l)
	(declare (ignore str))
	(when match
	  (let ((k (aref match 0))
		(v (aref match 1)))
	    (cond
	      ((string-match k "artists")
	       (setf (stats-artists stats) (safe-parse-integer v)))
	      ((string-match k "albums")
	       (setf (stats-albums stats) (safe-parse-integer v)))
	      ((string-match k "songs")
	       (setf (stats-songs stats) (safe-parse-integer v)))
	      ((string-match k "uptime")
	       (setf (stats-uptime stats) (safe-parse-integer v)))
	      ((string-match k "playtime")
	       (setf (stats-playtime stats) (safe-parse-integer v)))
	      ((string-match k "db_playtime")
	       (setf (stats-db-playtime stats) (safe-parse-integer v)))
	      ((string-match k "db_update")
	       (setf (stats-db-update stats) (safe-parse-integer v))))))))
    stats))

(defun status-result-handler (result)
  "This handler returns an instance of the mpd-status class"
  (let ((status (make-instance 'mpd-status)))
    (dolist (l result)
      (multiple-value-bind (str match) (scan-to-strings +key-value-pair-re+ l)
	(declare (ignore str))
	(when match
	  (let ((k (aref match 0))
		(v (aref match 1)))
	    (cond
	      ((string-match k "volume")
	       (setf (status-volume status) (safe-parse-integer v)))
	      ((string-match k "repeat")
	       (setf (status-repeat status) (safe-parse-integer v)))
	      ((string-match k "random")
	       (setf (status-random status) (safe-parse-integer v)))
	      ((string-match k "playlist")
	       (setf (status-playlist status) (safe-parse-integer v)))
	      ((string-match k "playlistlength")
	       (setf (status-playlist-length status) (safe-parse-integer v)))
	      ((string-match k "crossfade")
	       (setf (status-crossfade status) (safe-parse-integer v)))
	      ((string-match k "state")
	       (setf (status-state status) (or (and (equal v "play") :play)
					       (and (equal v "stop") :stop)
					       (and (equal v "pause") :pause)))))))))
    status))

(defun hash-result-handler (result)
  "This handler treat results which contain lists of files and directories"
  (let ((h (make-hash-table)))
    (dolist (line result)
      (multiple-value-bind (str match) (scan-to-strings +key-value-pair-re+ line)
	(declare (ignore str))
	(when match
	  (let ((k (aref match 0))
		(v (aref match 1)))
	    (cond
	     ((equal k "directory") (push v (gethash 'directory h)))
	     ((equal k "file") (push v (gethash 'file h)))
	     ((equal k "playlist") (push v (gethash 'playlist h))))))))
    h))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; COMMAND STUFF ;;;;
;;;;;;;;;;;;;;;;;;;;;;;
(defun send-command (connection command)
  "Send command to MPD via connection"
  (let ((io (iostream-of connection)))
    (write-string command io)
    (write-char #\Newline io)
    (force-output io)))

(defun build-command (cmd &rest args)
  "Build command using cmd and optional args"
  (with-output-to-string (s)
    (format s "~a" cmd)
    (mapcar #'(lambda (arg)
		(when arg
		  (if (stringp arg)
		      (format s " \"~a\"" arg)
		      (format s " ~a" arg)))) args)))

(defun handle-error (line)
  "Check if the line is an error message and raise a mpd-error accordingly"
  (let ((errmsg (regexp-match +error-re+ line)))
    (when errmsg
      (error 'mpd-error :message errmsg))))

(defun get-results (connection)
  "Read results of previously executed command from connection"
  (loop
     for line = (read-line (iostream-of connection) nil nil)
     while line
     until (string= line "OK")
     collect line into lines
     do (handle-error line)
     finally (return lines)))

(defmacro mpd-command (&key name (args nil) (command nil) (result-handler 'default-result-handler) (documentation ""))
  "Generate code for command"
  (let ((mpd-command (or command (string-downcase (symbol-name name))))
	(arguments (remove-if #'(lambda (arg) (eq arg '&optional)) args)))
    `(defun ,name (connection ,@args)
       ,documentation
       (reconnect connection)
       (send-command connection (build-command ,mpd-command ,@arguments))
       (,result-handler (get-results connection)))))

;;;;;;;;;;;;;;;;;;;;
;;;; CONNECTION ;;;;
;;;;;;;;;;;;;;;;;;;;
;;; FIXME only reconnect when needed
(defun reconnect (connection)
  "Reconnect connection when needed"
  (let ((s (socket-connect (get-server connection)
			   (get-port connection))))
    (with-slots (iostream version) connection
      (setf iostream (socket-stream s)
	    version (get-version (read-line iostream nil nil))))
    connection))

(defun connect (&optional (server +default-hostname+) (port +default-port+))
  "Connect to MPD server on server:port (defaults to localhost:6600)"
  (let ((c (make-instance 'mpd-connection :server server :port port)))
    (reconnect c)))

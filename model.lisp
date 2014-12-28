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

;;;; MODEL
(defclass mpd-connection ()
  ((server :reader get-server :initarg :server :initform +default-hostname+)
   (port :reader get-port :initarg :port :initform +default-port+)
   (password)
   (iostream :accessor iostream-of :initform nil)
   (version :accessor version-of :initform nil)))

(defmethod print-object ((c mpd-connection) stream)
    (print-unreadable-object (c stream :type t)
      (with-slots (server port iostream) c
	  (format stream ":SERVER ~a :PORT ~a :CONNECTED ~a" server port (not (null iostream))))))

(defclass mpd-song ()
  ((id :accessor song-id)
   (pos :accessor song-position)
   (file :accessor song-file)
   (title :accessor song-title)
   (album :accessor song-album)
   (artist :accessor song-artist)
   (time :accessor song-time)
   (track :accessor song-track)
   (genre :accessor song-genre)))

(defmethod print-object ((s mpd-song) stream)
  (print-unreadable-object (s stream :type t)
    (with-slots (id pos file title album artist time track genre) s
      (format stream ":ID ~a :POS ~a :FILE ~S :TITLE ~S :ALBUM ~S :ARTIST ~S :TIME ~a :TRACK ~a :GENRE ~a" id pos file title album artist time track genre))))

(defclass mpd-stats ()
  ((artists :accessor stats-artists :initarg :artists :initform 0)
   (albums :accessor stats-albums :initarg :albums :initform 0)
   (songs :accessor stats-songs :initarg :songs :initform 0)
   (uptime :accessor stats-uptime :initarg :songs :initform 0)
   (playtime :accessor stats-playtime :initarg :playtime :initform 0)
   (db-playtime :accessor stats-db-playtime :initarg :db-playtime :initform 0)
   (db-update :accessor stats-db-update :initarg :db-update :initform 0)))

(defmethod print-object ((s mpd-stats) stream)
  (print-unreadable-object (s stream :type t)
    (with-slots (artists albums songs uptime playtime db-playtime db-update) s
      (format stream ":ARTISTS ~d :ALBUMS ~d :SONGS ~d :UPTIME ~d :PLAYTIME ~d :DB-PLAYTIME ~d :DB-UPDATE ~d"
	      artists albums songs uptime playtime db-playtime db-update))))

(defclass mpd-status ()
  ((volume :accessor status-volume :initform 0)
   (repeat :accessor status-repeat :initform 0)
   (random :accessor status-random :initform 0)
   (playlist :accessor status-playlist :initform 0)
   (playlist-length :accessor status-playlist-length :initform 0)
   (crossfade :accessor status-crossfade :initform 0)
   (state :accessor status-state :initform :stop)))

(defmethod print-object ((s mpd-status) stream)
  (print-unreadable-object (s stream :type t)
    (with-slots (volume repeat random playlist playlist-length crossfade state) s
      (format stream ":STATE ~a :REPEAT ~a :RANDOM ~a :VOLUME ~d :CROSSFADE ~d :PLAYLIST ~d :PLAYLIST-LENGTH ~d"
	      state (equal repeat 1) (equal random 1) volume crossfade playlist playlist-length))))

;;;; CONDITIONS
(define-condition mpd-error (error)
  ((message :initarg :message :reader message)))

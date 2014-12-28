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


(defpackage #:mpd-client
  (:use #:cl #:usocket #:cl-ppcre)
  (:export :connect
	   ;; mpd-song accessors
	   :song-id :song-position :song-file :song-album :song-artist :song-title :song-track :song-time :song-genre
	   ;; mpd-stats accessors
	   :stats-artists :stats-albums :stats-songs :stats-uptime :stats-playtime :stats-db-playtime :stats-db-update
	   ;; mpd commands
	   :add
	   :clear
	   :clearerror
	   :crossfade
	   :current-song
	   :delete-song
	   :delete-song/id
	   :find/album
	   :find/artist
	   :find/title
	   :kill
	   :list/album
	   :list/artist
	   :list-all
	   :list-all-info
	   :load-playlist
	   :lsinfo
	   :next
	   :pause
	   :ping
	   :play
	   :play/id
	   :playlist-info
	   :playlist-info/id
	   :playlist-changes
	   :playlist-changes/id
	   :previous
	   :random
	   :repeat
	   :rm
	   :save
	   :search/title
	   :search/artist
	   :search/album
	   :search/filename
	   :seek
	   :seek/id
	   :set-volume
	   :shuffle
	   :stats
	   :status
	   :stop
	   :swap
	   :swap/id
	   :tagtypes
	   :update))

(in-package #:mpd-client)

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

;; All MPD commands are implemented in this file

(in-package #:mpd-client)

(mpd-command :name add
	     :args (path)
	     :documentation "add the file _path_ to the playlist (directories add recursively)")

(mpd-command :name clear)

(mpd-command :name clearerror)

(mpd-command :name crossfade 
	     :args (seconds))

(mpd-command :name current-song)

(mpd-command :name delete-song
	     :args (song)
	     :command "delete")

(mpd-command :name delete-song/id
	     :args (songid)
	     :command "deleteid")

(mpd-command :name find/album 
	     :args (what)
	     :command "find album"
	     :result-handler song-info-result-handler)

(mpd-command :name find/artist
	     :args (what)
	     :command "find artist"
	     :result-handler song-info-result-handler)

(mpd-command :name find/title
	     :args (what)
	     :command "find title"
	     :result-handler song-info-result-handler)

(mpd-command :name kill)

(mpd-command :name list/album
	     :args (&optional artist)
	     :command "list album"
	     :result-handler pair-result-handler)

(mpd-command :name list/artist
	     :command "list artist"
	     :result-handler pair-result-handler)

(mpd-command :name list-all
	     :args (&optional path)
	     :command "listall"
	     :result-handler hash-result-handler)

;; TODO handle result
(mpd-command :name list-all-info
	     :args (&optional path)
	     :command "listallinfo")

(mpd-command :name load-playlist
	     :args (name)
	     :command "load")

(mpd-command :name lsinfo
	     :args (directory)
	     :result-handler hash-result-handler)

(mpd-command :name next)

(mpd-command :name ping)

(mpd-command :name play
	     :args (&optional song))

(mpd-command :name play/id 
	     :args (&optional song)
	     :command "playid")

(mpd-command :name playlist-info
	     :args (&optional song)
	     :command "playlistinfo"
	     :result-handler song-info-result-handler)

(mpd-command :name playlist-info/id
	     :args (&optional songid)
	     :command "playlistid"
	     :result-handler song-info-result-handler)

(mpd-command :name playlist-changes
	     :args (version)
	     :command "plchanges"
	     :result-handler song-info-result-handler)

;; TODO handle result
(mpd-command :name playlist-changes/id
	     :args (version)
	     :command "plchangesposid"
	     :result-handler identity-result-handler)

(mpd-command :name previous)

(mpd-command :name rm
	     :args (name))

(mpd-command :name save
	     :args (name))

(mpd-command :name search/title
	     :args (what)
	     :command "search title"
	     :result-handler song-info-result-handler)

(mpd-command :name search/artist
            :args (what)
	    :command "search artist"
	    :result-handler song-info-result-handler)

(mpd-command :name search/album
	     :args (what)
	     :command "search album"
	     :result-handler song-info-result-handler)

(mpd-command :name search/filename
	     :args (what) 
	     :command "search filename"
	     :result-handler song-info-result-handler)

(mpd-command :name seek
	     :args (song time))

(mpd-command :name seek/id 
	     :args (songid time)
	     :command "seekid")

(mpd-command :name shuffle)

(mpd-command :name stats
	     :result-handler stats-result-handler)

(mpd-command :name status
	     :result-handler status-result-handler)

(mpd-command :name stop)

(mpd-command :name swap
	     :args (song1 song2))

(mpd-command :name swap/id
	     :args (songid1 songid2)
	     :command "swapid")

(mpd-command :name update
	     :args (&optional path))

(mpd-command :name tagtypes 
	     :result-handler pair-result-handler)

;; XXX switch to mpd-command macro
(defun pause (connection pause)
  "Toggle pause/resume playing"
  (reconnect connection)
  (send-command connection (build-command "pause" (or (and pause 1) 0)))
  (get-results connection))

;; XXX switch to mpd-command macro
(defun set-random (connection state)
  "Set random state"
  (reconnect connection)
  (send-command connection (build-command "random" (or (and state 1) 0)))
  (get-results connection))

;; XXX switch to mpd-command macro
(defun repeat (connection state)
  "Set repeat state"
  (reconnect connection)
  (send-command connection (build-command "repeat" (or (and state 1) 0)))
  (get-results connection))

;; XXX switch to mpd-command macro
(defun set-volume (connection volume)
  "Set volume to _volume_ whose range is 0-100"
  (reconnect connection)
  (send-command connection (build-command "setvol" (max 0 (min volume 100))))
  (get-results connection))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-mulitmedia.lisp                                                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-multimedia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-multimedia (clog-element)()
  (:documentation "CLOG Multimedia base class."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-multimedia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;
;; loop-mediap ;;
;;;;;;;;;;;;;;;;;

(defgeneric loop-mediap (clog-multimedia)
  (:documentation "Get/Setf loop media property."))

(defmethod loop-mediap ((obj clog-multimedia))
  (js-true-p (property obj "loop")))

(defgeneric set-loop-mediap (clog-multimedia value)
  (:documentation "Set loop-media VALUE for CLOG-MULTIMEDIA"))

(defmethod set-loop-mediap ((obj clog-multimedia) value)
  (setf (property obj "loop") (p-true-js value)))
(defsetf loop-mediap set-loop-mediap)

;;;;;;;;;;;;;;;;;;;;
;; media-duration ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric media-duration (clog-multimedia)
  (:documentation "Get/Setf media in seconds property."))

(defmethod media-duration ((obj clog-multimedia))
  (parse-float (property obj "duration" :default-answer 0) :type 'double-float :junk-allowed t))

;;;;;;;;;;;;;;;;;;
;; media-source ;;
;;;;;;;;;;;;;;;;;;

(defgeneric media-source (clog-multimedia)
  (:documentation "Get/Setf media source/url."))

(defmethod media-source ((obj clog-multimedia))
  (property obj "src"))

(defgeneric set-media-source (clog-multimedia value)
  (:documentation "Set media source VALUE for CLOG-MULTIMEDIA"))

(defmethod set-media-source ((obj clog-multimedia) value)
  (setf (property obj "src") value))
(defsetf media-source set-media-source)

;;;;;;;;;;;;;;;;;;;;
;; media-position ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric media-position (clog-multimedia)
  (:documentation "Get/Setf postion of media in seconds."))

(defmethod media-position ((obj clog-multimedia))
  (parse-float (property obj "currentTime" :default-answer 0) :type 'double-float :junk-allowed t))

(defgeneric set-media-position (clog-multimedia value)
  (:documentation "Set media source VALUE for CLOG-MULTIMEDIA"))

(defmethod set-media-position ((obj clog-multimedia) value)
  (setf (property obj "currentTime") value))
(defsetf media-position set-media-position)

;;;;;;;;;;;;
;; mutedp ;;
;;;;;;;;;;;;

(defgeneric mutedp (clog-multimedia)
  (:documentation "Get/Setf muted property."))

(defmethod mutedp ((obj clog-multimedia))
  (js-true-p (property obj "muted")))

(defgeneric set-mutedp (clog-multimedia value)
  (:documentation "Set muted VALUE for CLOG-MULTIMEDIA"))

(defmethod set-mutedp ((obj clog-multimedia) value)
  (setf (property obj "muted") (p-true-js value)))
(defsetf mutedp set-mutedp)

;;;;;;;;;;;;;
;; pausedp ;;
;;;;;;;;;;;;;

(defgeneric pausedp (clog-multimedia)
  (:documentation "Get/Setf paused property."))

(defmethod pausedp ((obj clog-multimedia))
  (js-true-p (property obj "paused")))

;;;;;;;;;;;;;;
;; seekingp ;;
;;;;;;;;;;;;;;

(defgeneric seekingp (clog-multimedia)
  (:documentation "Get/Setf seeking property."))

(defmethod seekingp ((obj clog-multimedia))
  (js-true-p (property obj "seeking")))

;;;;;;;;;;;;;;;;;;;;;;
;; playback-ended-p ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric playback-ended-p (clog-multimedia)
  (:documentation "Get/Setf true of Media position has reached end of its
duration."))

(defmethod playback-ended-p ((obj clog-multimedia))
  (js-true-p (property obj "ended")))

;;;;;;;;;;;;;;;;;;;
;; playback-rate ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric playback-rate (clog-multimedia)
  (:documentation "Get/Setf playback rate.
Common values - 1.0 normal, 0.5 half speed, -1.0 reverse"))

(defmethod playback-rate ((obj clog-multimedia))
  (property obj "playbackRate"))

(defgeneric set-playback-rate (clog-multimedia value)
  (:documentation "Set media source VALUE for CLOG-MULTIMEDIA"))

(defmethod set-playback-rate ((obj clog-multimedia) value)
  (setf (property obj "playbackRate") value))
(defsetf playback-rate set-playback-rate)

;;;;;;;;;;;;;;;;;;;;;
;; ready-to-play-p ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric ready-to-play-p (clog-multimedia)
  (:documentation "Get/Setf true of Media position has reached end of its
duration."))

(defmethod ready-to-play-p ((obj clog-multimedia))
  (js-true-p (property obj "readyState")))

;;;;;;;;;;;;;;;;;;
;; media-volume ;;
;;;;;;;;;;;;;;;;;;

(defgeneric media-volume (clog-multimedia)
  (:documentation "Get/Setf media volume, not system volume. 0.0 .. 1.0"))

(defmethod media-volume ((obj clog-multimedia))
  (parse-float (property obj "volume" :default-answer 0) :type 'double-float :junk-allowed t))

(defgeneric set-media-volume (clog-multimedia value)
  (:documentation "Set media source VALUE for CLOG-MULTIMEDIA"))

(defmethod set-media-volume ((obj clog-multimedia) value)
  (setf (property obj "volume") value))
(defsetf media-volume set-media-volume)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-multimedia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;; play-media ;;
;;;;;;;;;;;;;;;;

(defgeneric play-media (clog-multimedia)
  (:documentation "Play media."))

(defmethod play-media ((obj clog-multimedia))
  (execute obj "play()"))

;;;;;;;;;;;;;;;;;
;; pause-media ;;
;;;;;;;;;;;;;;;;;

(defgeneric pause-media (clog-multimedia)
  (:documentation "Pause media."))

(defmethod pause-media ((obj clog-multimedia))
  (execute obj "pause()"))

;;;;;;;;;;;;;;;;
;; load-media ;;
;;;;;;;;;;;;;;;;

(defgeneric load-media (clog-multimedia)
  (:documentation "Load/Reload media."))

(defmethod load-media ((obj clog-multimedia))
  (execute obj "load()"))

;;;;;;;;;;;;;;;;;;;;;
;; can-play-type-p ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric can-play-type-p (clog-multimedia media-type)
  (:documentation "Returns true if browser claims support of a media type.
Browsers report possibility but not guarantees of being able to support a
media type.

     Common values:
       video/ogg
       video/mp4
       video/webm
       audio/mpeg
       audio/ogg
       audio/mp4
       audio/mp3

     Common values, including codecs:
       video/ogg; codecs=\"theora, vorbis\"
       video/mp4; codecs=\"avc1.4D401E, mp4a.40.2\"
       video/webm; codecs=\"vp8.0, vorbis\"
       audio/ogg; codecs=\"vorbis\"
       audio/mp4; codecs=\"mp4a.40.5\""))

(defmethod can-play-type-p ((obj clog-multimedia) media-type)
  (js-true-p (query obj (format nil "canPlayType('~A')" media-type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events - clog-multimedia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-media-abort ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-media-abort (clog-multimedia handler)
  (:documentation "Set the ON-MEDIA-ABORT-HANDLER for CLOG-MULTIMEDIA. HANDLER
is nil unbind the event."))

(defmethod set-on-media-abort ((obj clog-multimedia) handler)
  (set-event obj "abort"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-media-error ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-media-error (clog-multimedia handler)
  (:documentation "Set the ON-MEDIA-ERROR-HANDLER for CLOG-MULTIMEDIA. HANDLER
is nil unbind the event."))

(defmethod set-on-media-error ((obj clog-multimedia) handler)
  (set-event obj "error"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;;
;; set-on-can-play ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-can-play (clog-multimedia handler)
  (:documentation "Set the ON-CAN-PLAY-HANDLER for CLOG-MULTIMEDIA. HANDLER
is nil unbind the event."))

(defmethod set-on-can-play ((obj clog-multimedia) handler)
  (set-event obj "canplay"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-can-play-through ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-can-play-through (clog-multimedia handler)
  (:documentation "Set the ON-CAN-PLAY-THROUGH-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-can-play-through ((obj clog-multimedia) handler)
  (set-event obj "canplaythrough"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-duration-change ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-duration-change (clog-multimedia handler)
  (:documentation "Set the ON-DURATION-CHANGE-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-duration-change ((obj clog-multimedia) handler)
  (set-event obj "durationchange"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;
;; set-on-emptied ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-emptied (clog-multimedia handler)
  (:documentation "Set the ON-EMPTIED-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-emptied ((obj clog-multimedia) handler)
  (set-event obj "emptied"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;
;; set-on-ended ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-ended (clog-multimedia handler)
  (:documentation "Set the ON-ENDED-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-ended ((obj clog-multimedia) handler)
  (set-event obj "ended"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-loaded-data ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-loaded-data (clog-multimedia handler)
  (:documentation "Set the ON-LOADED-DATA-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-loaded-data ((obj clog-multimedia) handler)
  (set-event obj "loadeddata"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-loaded-meta-data ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-loaded-meta-data (clog-multimedia handler)
  (:documentation "Set the ON-LOADED-META-DATA-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-loaded-meta-data ((obj clog-multimedia) handler)
  (set-event obj "loadedmetadata"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-load-start ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-load-start (clog-multimedia handler)
  (:documentation "Set the ON-LOAD-START-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-load-start ((obj clog-multimedia) handler)
  (set-event obj "loadstart"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;
;; set-on-play ;;
;;;;;;;;;;;;;;;;;

(defgeneric set-on-play (clog-multimedia handler)
  (:documentation "Set the ON-PLAY-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-play ((obj clog-multimedia) handler)
  (set-event obj "play"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;
;; set-on-pause ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pause (clog-multimedia handler)
  (:documentation "Set the ON-PAUSE-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-pause ((obj clog-multimedia) handler)
  (set-event obj "pause"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;
;; set-on-playing ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-playing (clog-multimedia handler)
  (:documentation "Set the ON-PLAYING-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-playing ((obj clog-multimedia) handler)
  (set-event obj "playing"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;;
;; set-on-progress ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-progress (clog-multimedia handler)
  (:documentation "Set the ON-PROGRESS-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-progress ((obj clog-multimedia) handler)
  (set-event obj "progress"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-rate-change ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-rate-change (clog-multimedia handler)
  (:documentation "Set the ON-RATE-CHANGE-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-rate-change ((obj clog-multimedia) handler)
  (set-event obj "ratechange"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;
;; set-on-seeked ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-seeked (clog-multimedia handler)
  (:documentation "Set the ON-SEEKED-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-seeked ((obj clog-multimedia) handler)
  (set-event obj "seeked"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;
;; set-on-seeking ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-seeking (clog-multimedia handler)
  (:documentation "Set the ON-SEEKING-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-seeking ((obj clog-multimedia) handler)
  (set-event obj "seeking"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;
;; set-on-stalled ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-stalled (clog-multimedia handler)
  (:documentation "Set the ON-STALLED-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-stalled ((obj clog-multimedia) handler)
  (set-event obj "stalled"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;
;; set-on-suspend ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-suspend (clog-multimedia handler)
  (:documentation "Set the ON-SUSPEND-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-suspend ((obj clog-multimedia) handler)
  (set-event obj "suspend"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-time-update ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-time-update (clog-multimedia handler)
  (:documentation "Set the ON-TIME-UPDATE-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-time-update ((obj clog-multimedia) handler)
  (set-event obj "timeupdate"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-volume-change ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-volume-change (clog-multimedia handler)
  (:documentation "Set the ON-VOLUME-CHANGE-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-volume-change ((obj clog-multimedia) handler)
  (set-event obj "volumechange"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;
;; set-on-waiting ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-waiting (clog-multimedia handler)
  (:documentation "Set the ON-WAITING-HANDLER for CLOG-MULTIMEDIA.
HANDLER is nil unbind the event."))

(defmethod set-on-waiting ((obj clog-multimedia) handler)
  (set-event obj "waiting"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-audio
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-audio (clog-multimedia)()
  (:documentation "CLOG Audio class."))

;;;;;;;;;;;;;;;;;;
;; create-audio ;;
;;;;;;;;;;;;;;;;;;

(defgeneric create-audio (clog-obj &key
                                     source
                                     controls
                                     preload
                                     autoplay
                                     autoloop
                                     muted
                                     html-id
                                     auto-place)
  (:documentation "Create a CLOG Audio control"))

(defmethod create-audio ((obj clog-obj)
                         &key (source "")
                           (controls t)
                           (preload  nil)
                           (autoplay nil)
                           (autoloop nil)
                           (muted    nil)
                           (html-id  nil)
                           (auto-place t))
  (create-child obj (format nil "<audio~A~A~A~A~A~A/>"
                            (if (equal source "")
                                ""
                                (format nil " src='~A'"
                                        (escape-string source)))
                            (if controls
                                " controls"
                                "")
                            (if preload
                                " preload='auto'"
                                "")
                            (if autoplay
                                " autoplay"
                                "")
                            (if autoloop
                                " loop"
                                "")
                            (if muted
                                " muted"
                                ""))
                :clog-type  'clog-audio
                :html-id    html-id
                :auto-place auto-place))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-video
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-video (clog-multimedia)()
  (:documentation "CLOG Video class."))

(defgeneric create-video (clog-obj &key
                                     source
                                     controls
                                     preload
                                     poster
                                     autoplay
                                     autoloop
                                     muted
                                     html-id
                                     auto-place)
(:documentation "Create a CLOG video control"))

(defmethod create-video ((obj clog-obj)
                        &key (source "")
                          (controls t)
                          (preload  nil)
                          (poster   "")
                          (autoplay nil)
                          (autoloop nil)
                          (muted    nil)
                          (html-id  nil)
                          (auto-place t))
  (create-child obj (format nil "<video~A~A~A~A~A~A~A/>"
                            (if (equal source "")
                                ""
                                (format nil " src='~A'"
                                        (escape-string source)))
                            (if controls
                                " controls"
                                "")
                            (if preload
                                " preload='auto'"
                                "")
                            (if (equal poster "")
                                ""
                                (format nil " poster='~A'"
                                        (escape-string poster)))
                            (if autoplay
                                " autoplay"
                                "")
                            (if autoloop
                                " loop"
                                "")
                            (if muted
                                " muted"
                                ""))
                :clog-type  'clog-video
                :html-id    html-id
                :auto-place auto-place))

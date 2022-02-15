;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog.lisp                                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Defines the various exports using mgl-pax to auto generate documentation
;;; for clog. The physical files map in most cases to the defsections with
;;; the exception of clog-obj which is defined in clog-base.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports - clog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mgl-pax:define-package :clog
  (:documentation "The Common List Omnificent GUI - CLOG")
  (:use #:cl #:parse-float #:mgl-pax))

(cl:in-package :clog)

(defsection @clog-manual (:title "The CLOG manual")
  "The Common Lisp Omnificient GUI, CLOG for short, uses web technology to
produce graphical user interfaces for applications locally or remotely.
CLOG can take the place, or work along side, most cross platform GUI
frameworks and website frameworks. The CLOG package starts up the
connectivity to the browser or other websocket client (often a browser
embedded in a native template application.)"

  (@clog-getting-started    section)
  (@clog-programming-basics section)
  (@clog-event-data         section)

  (@clog-system          section)
  (@clog-utilities       section)
  (@clog-obj             section)
  (@clog-element         section)
  (@clog-element-common  section)
  (@clog-presentations   section)
  (@clog-panels          section)
  (@clog-style-block     section)
  (@clog-form            section)
  (@clog-canvas          section)
  (@clog-multimedia      section)
  (@clog-gui             section)
  (@clog-web             section)
  (@clog-body            section)
  (@clog-window          section)
  (@clog-document        section)
  (@clog-location        section)
  (@clog-navigator       section)
  (@clog-helpers         section)
  (@clog-internals       section))


(defsection @clog-system (:title "CLOG System")
  "CLOG Startup and Shutdown"
  (initialize        function)
  (set-on-new-window function)
  (shutdown          function)
  (debug-mode        function)
  (open-browser      function))

(defsection @clog-utilities (:title "CLOG Utilities")
  "Concurrent Hash Tables"
  (make-hash-table* function)

  "CLOG-Group - Utility Class for CLOG-Obj storage"
  (clog-group    class)
  (create-group  function)
  (add           generic-function)
  (obj           generic-function)

  "CLOG JS utilities"
  (js-true-p     function)
  (p-true-js     function)
  (js-on-p       function)
  (p-on-js       function)
  (escape-string function)
  (lf-to-br      function)

  "CLOG Color utilities"
  (rgb           function)
  (rgba          function)
  (hsl           function)
  (hsla          function)

  "CLOG Unit utilities"
  (unit          function))

(defsection @clog-obj (:title "CLOG Objects")
  "CLOG-Obj - Base class for CLOG Objects"
  (clog-obj class)

  "CLOG-Obj - General Properties"
  (property  generic-function)

  "CLOG-Obj - General Methods"
  (height generic-function)
  (width  generic-function)
  (focus  generic-function)
  (blur   generic-function)

  "CLOG-Obj - Low Level"
  (connection-data             generic-function)
  (connection-data-item        generic-function)
  (connection-body             generic-function)
  (connection-sync             generic-function)
  (with-sync-event             macro)
  (remove-connection-data-item generic-function)
  (validp                      generic-function)

  "CLOG-Obj - Internals for Extension Plugins"
  (html-id    generic-function)
  (js-execute generic-function)
  (js-query   generic-function)

  "CLOG-Obj - Event Handling"
  (set-on-resize             generic-function)
  (set-on-focus              generic-function)
  (set-on-blur               generic-function)
  (set-on-change             generic-function)
  (set-on-drag-start         generic-function)
  (set-on-drag               generic-function)
  (set-on-drag-end           generic-function)
  (set-on-drag-enter         generic-function)
  (set-on-drag-leave         generic-function)
  (set-on-drag-over          generic-function)
  (set-on-drop               generic-function)
  (set-on-focus-in           generic-function)
  (set-on-focus-out          generic-function)
  (set-on-reset              generic-function)
  (set-on-search             generic-function)
  (set-on-select             generic-function)
  (set-on-submit             generic-function)
  (set-on-select             generic-function)
  (set-on-context-menu       generic-function)
  (set-on-click              generic-function)
  (set-on-double-click       generic-function)
  (set-on-mouse-click        generic-function)
  (set-on-mouse-double-click generic-function)
  (set-on-mouse-right-click  generic-function)
  (set-on-mouse-enter        generic-function)
  (set-on-mouse-leave        generic-function)
  (set-on-mouse-over         generic-function)
  (set-on-mouse-out          generic-function)
  (set-on-mouse-down         generic-function)
  (set-on-mouse-up           generic-function)
  (set-on-mouse-move         generic-function)
  (set-on-pointer-enter      generic-function)
  (set-on-pointer-leave      generic-function)
  (set-on-pointer-over       generic-function)
  (set-on-pointer-out        generic-function)
  (set-on-pointer-down       generic-function)
  (set-on-pointer-up         generic-function)
  (set-on-pointer-move       generic-function)
  (set-on-touch-start        generic-function)
  (set-on-touch-move         generic-function)
  (set-on-touch-end          generic-function)
  (set-on-touch-cancel       generic-function)
  (set-on-character          generic-function)
  (set-on-key-down           generic-function)
  (set-on-key-up             generic-function)
  (set-on-key-press          generic-function)
  (set-on-copy               generic-function)
  (set-on-cut                generic-function)
  (set-on-paste              generic-function))

(defsection @clog-element (:title "CLOG Elements")
  "CLOG-Element - Class for CLOG Elements"
  (clog-element class)

  "CLOG-Element - Low Level Creation"
  (create-child    generic-function)
  (attach-as-child generic-function)

  "CLOG-Element - DOM Placement"
  (place-after            generic-function)
  (place-before           generic-function)
  (place-inside-top-of    generic-function)
  (place-inside-bottom-of generic-function)

  "CLOG-Element - General Properties"
  (style            generic-function)
  (set-styles       generic-function)
  (attribute        generic-function)
  (remove-attribute generic-function)

  "CLOG-Element - Properties"
  (access-key          generic-function)
  (advisory-title      generic-function)
  (css-class-name      generic-function)
  (editablep           generic-function)
  (draggablep          generic-function)
  (visiblep            generic-function)
  (hiddenp             generic-function)
  (inner-html          generic-function)
  (outer-html          generic-function)
  (spellcheckp         generic-function)
  (tab-index           generic-function)
  (text                generic-function)
  (text-direction-type type)
  (text-direction      generic-function)
  (language-code       generic-function)
  (position-left       generic-function)
  (position-top        generic-function)
  (client-left         generic-function)
  (client-top          generic-function)
  (client-width        generic-function)
  (client-height       generic-function)
  (offset-left         generic-function)
  (offset-top          generic-function)
  (offset-width        generic-function)
  (offset-height       generic-function)
  (scroll-left         generic-function)
  (scroll-top          generic-function)
  (scroll-width        generic-function)
  (scroll-height       generic-function)
  (html-tag            generic-function)

  "CLOG-Element - Styles"
  (box-sizing-type            type)
  (box-sizing                 generic-function)
  (clear-side-type            type)
  (clear-side                 generic-function)
  (float-wrap-type            type)
  (float-wrap                 generic-function)
  (display-type               type)
  (display                    generic-function)
  (order                      generic-function)
  (flex                       generic-function)
  (set-flex                   generic-function)
  (flex-wrap-type             type)
  (flex-wrap                  generic-function)
  (flex-direction-type        type)
  (flex-direction             generic-function)
  (grid-template-columns      generic-function)
  (grid-template-rows         generic-function)
  (grid-template-areas        generic-function)
  (column-gap                 generic-function)
  (row-gap                    generic-function)
  (grid-auto-columns          generic-function)
  (grid-auto-rows             generic-function)
  (grid-auto-flow             generic-function)
  (grid-column-start          generic-function)
  (grid-column-end            generic-function)
  (grid-row-start             generic-function)
  (grid-row-end               generic-function)
  (align-items-type           type)
  (align-items                generic-function)
  (align-self-type            type)
  (align-self                 generic-function)
  (align-content-type         type)
  (align-content              generic-function)
  (justify-items-type         type)
  (justify-items              generic-function)
  (justify-self-type          type)
  (justify-self               generic-function)
  (justify-content-type       type)
  (justify-content            generic-function)
  (overflow-type              type)
  (overflow                   generic-function)
  (overflow-x-type            type)
  (overflow-x                 generic-function)
  (overflow-y-type            type)
  (overflow-y                 generic-function)
  (z-index                    generic-function)
  (resizable-type             type)
  (resizable                  generic-function)
  (positioning-type           type)
  (positioning                generic-function)
  (position-top               generic-function)
  (position-left              generic-function)
  (set-geometry               generic-function)
  (left                       generic-function)
  (top                        generic-function)
  (right                      generic-function)
  (bottom                     generic-function)
  (box-height                 generic-function)
  (box-width                  generic-function)
  (maximum-height             generic-function)
  (maximum-width              generic-function)
  (minimum-height             generic-function)
  (minimum-width              generic-function)
  (inner-height               generic-function)
  (inner-width                generic-function)
  (outer-height               generic-function)
  (outer-width                generic-function)
  (outer-height-to-margin     generic-function)
  (outer-width-to-margin      generic-function)
  (color                      generic-function)
  (opacity                    generic-function)
  (background-attachment-type type)
  (background-attachment      generic-function)
  (background-color           generic-function)
  (background-image           generic-function)
  (background-position        generic-function)
  (background-origin-type     type)
  (background-origin          generic-function)
  (background-repeat-type     type)
  (background-repeat          generic-function)
  (background-clip-type       type)
  (background-clip            generic-function)
  (background-size            generic-function)
  (border-style-type          type)
  (border                     generic-function)
  (set-border                 generic-function)
  (border-radius              generic-function)
  (box-shadow                 generic-function)
  (text-shadow                generic-function)
  (outline-style-type         type)
  (outline                    generic-function)
  (set-outline                generic-function)
  (margin                     generic-function)
  (set-margin                 generic-function)
  (set-margin-side            generic-function)
  (padding                    generic-function)
  (set-padding                generic-function)
  (set-padding-side           generic-function)
  (cursor                     generic-function)
  (font-style-type            type)
  (font-variant-type          type)
  (system-font-type           type)
  (font                       generic-function)
  (set-font                   generic-function)
  (text-alignment-type        type)
  (text-alignment             generic-function)
  (vertical-align-type        type)
  (vertical-align             generic-function)

  "CLOG-Element - Methods"
  (add-class        generic-function)
  (remove-class     generic-function)
  (toggle-class     generic-function)
  (remove-from-dom  generic-function)
  (remove-from-clog generic-function)
  (destroy          generic-function)
  (click            generic-function)

  "CLOG-Element - DOM Traversal Methods"
  (parent-element   generic-function)
  (first-child      generic-function)
  (previous-sibling generic-function)
  (next-sibling     generic-function))

(defsection @clog-element-common (:title "Common CLOG Elements")
  "CLOG-A - Class for CLOG Anchors"
  (clog-a   class)
  (create-a generic-function)
  (link     generic-function)
  (target   generic-function)

  "CLOG-BR - Class for CLOG Line Breaks"
  (clog-br   class)
  (create-br generic-function)

  "CLOG-BUTTON - Class for CLOG Buttons"
  (clog-button   class)
  (create-button generic-function)
  (disabledp     generic-function)

  "CLOG-IMG - Class for CLOG Images"
  (clog-img   class)
  (create-img generic-function)
  (url-src    generic-function)

  "CLOG-Div - Class for CLOG Div Blocks"
  (clog-div   class)
  (create-div generic-function)

  "CLOG-Dialog - Class for CLOG Dialog Blocks"
  (clog-dialog   class)
  (create-dialog generic-function)
  (return-value  generic-function)
  (dialog-openp  generic-function)
  (show-dialog   generic-function)
  (close-dialog  generic-function)

  "CLOG-Details - Class for CLOG Detail Blocks"
  (clog-details   class)
  (create-details generic-function)
  (details-openp  generic-function)

  "CLOG-Summary - Class for CLOG Summary Blocks"
  (clog-summary   class)
  (create-summary generic-function)

  "CLOG-HR - Class for CLOG Hortizontal Rules"
  (clog-HR   class)
  (create-HR generic-function)

  "CLOG-Meter - Class for CLOG Meters"
  (clog-meter   class)
  (create-meter generic-function)
  (value        generic-function)
  (high         generic-function)
  (low          generic-function)
  (minimum      generic-function)
  (maximum      generic-function)
  (optimum      generic-function)

  "CLOG-Progress-Bar - Class for CLOG Progress Bars"
  (clog-progress-bar   class)
  (create-progress-bar generic-function)
  (value               generic-function)
  (maximum             generic-function)

  "CLOG-P - Class for CLOG Paragraphs"
  (clog-p   class)
  (create-p generic-function)

  "CLOG-Span - Class for CLOG Inline Spans"
  (clog-span   class)
  (create-span generic-function)

  "CLOG-Section - Class for CLOG Inline Sections"
  (section-type   type)
  (clog-section   class)
  (create-section generic-function)

  "CLOG-Phrase - Class for CLOG Inline Phrases"
  (phrase-type   type)
  (clog-phrase   class)
  (create-phrase generic-function)

  "CLOG-Ordered-List - Class for CLOG Ordered-Lists"
  (clog-ordered-list   class)
  (create-ordered-list generic-function)
  (list-kind-type      type)
  (list-kind           generic-function)
  (list-location-type  type)
  (list-location       generic-function)

  "CLOG-Unordered-List - Class for CLOG Unordered-Lists"
  (clog-unordered-list   class)
  (create-unordered-list generic-function)

  "CLOG-List-Item - Class for CLOG List-Items"
  (clog-list-item   class)
  (create-list-item generic-function)
  (item-value       generic-function)

  "CLOG-Definition-List - Class for CLOG Definition-Lists"
  (clog-definition-list   class)
  (create-definition-list generic-function)

  "CLOG-Term - Class for CLOG Terms"
  (clog-term   class)
  (create-term generic-function)

  "CLOG-Description - Class for CLOG Descriptions"
  (clog-description   class)
  (create-description generic-function)

  "CLOG-Table - Class for CLOG Tables"
  (clog-table   class)
  (create-table generic-function)

  "CLOG-Table-Row - Class for CLOG Table-Rows"
  (clog-table-row   class)
  (create-table-row generic-function)

  "CLOG-Table-Column - Class for CLOG Table-Columns"
  (clog-table-column   class)
  (create-table-column generic-function)

  "CLOG-Table-Heading - Class for CLOG Table-Headings"
  (clog-table-heading   class)
  (create-table-heading generic-function)

  "CLOG-Table-Head - Class for CLOG Table-Heads"
  (clog-table-head   class)
  (create-table-head generic-function)

  "CLOG-Table-Body - Class for CLOG Table-Bodys"
  (clog-table-body   class)
  (create-table-body generic-function)

  "CLOG-Table-Caption - Class for CLOG Table-Captions"
  (clog-table-caption   class)
  (create-table-caption generic-function)

  "CLOG-Table-Footer - Class for CLOG Table-Footers"
  (clog-table-footer   class)
  (create-table-footer generic-function)

  "CLOG-Table-Column-Group - Class for CLOG Table-Column-Groups"
  (clog-table-column-group   class)
  (create-table-column-group generic-function)

  "CLOG-Table-Column-Group-Item - Class for CLOG Table-Column-Group-Items"
  (clog-table-column-group-item   class)
  (create-table-column-group-item generic-function))

(defsection @clog-presentations (:title "CLOG Presentations")
  "CLOG-Presentations - CLOG Presentations"
  (link-slot-and-form-element      macro)
  (link-slot-and-element           macro)
  (link-form-element-to-slot       macro)
  (link-element-to-slot            macro)
  (link-element-to-place           macro)
  (link-slot-to-form-element       macro)
  (link-slot-to-element            macro)
  (link-slot-to-place              macro))

(defsection @clog-panels (:title "CLOG Panels")
  "CLOG-Panel - CLOG Panels"
  (clog-panel   class)
  (create-panel generic-function)

  "CLOG-Panel-Box - CLOG Panel Box"
  (clog-panel-box   class)
  (create-panel-box generic-function)
  (panel-box        generic-function)

  "CLOG-Panel-Box-Layout"
  (clog-panel-box-layout   class)
  (center-children         generic-function)
  (create-panel-box-layout function)
  (center-panel            generic-function)
  (top-panel               generic-function)
  (left-panel              generic-function)
  (right-panel             generic-function)
  (bottom-panel            generic-function))

(defsection @clog-style-block (:title "CLOG Style Blocks")
  "CLOG-Style-Block - CLOG Style Blocks"
  (clog-style-block    class)
  (create-style-block  generic-function)
  (add-style           generic-function))

(defsection @clog-form (:title "CLOG Form Objects")
  "CLOG-Form-Data"
  (form-get-data         generic-function)
  (form-post-data        generic-function)
  (form-multipart-data   generic-function)
  (delete-multipart-data generic-function)
  (form-data-item        function)

  "CLOG-Form - Class for organizing Form Elements in to a From"
  (form-method-type   type)
  (clog-form          class)
  (create-form        generic-function)

  (form-element-count generic-function)
  (submit             generic-function)
  (reset              generic-function)
  (autocompletep      generic-function)
  (encoding           generic-function)
  (validate-on-submit generic-function)

  "CLOG-Fieldset - Class for CLOG Fieldsets"
  (clog-fieldset   class)
  (create-fieldset generic-function)

  "CLOG-Legend - Class for CLOG Legends"
  (clog-legend   class)
  (create-legend generic-function)

  "CLOG-Form-Element - Class for form elements"
  (clog-form-element   class)
  (form-element-type   type)
  (create-form-element generic-function)

  (autocomplete        generic-function)
  (autofocusp          generic-function)
  (place-holder        generic-function)
  (disabledp           generic-function)
  (read-only-p         generic-function)
  (requiredp           generic-function)
  (multiplep           generic-function)
  (name                generic-function)
  (default-value       generic-function)
  (value               generic-function)
  (radio-value         generic-function)
  (checkbox-value      generic-function)
  (select-value        generic-function)
  (name-value          generic-function)
  (pattern             generic-function)
  (minimum             generic-function)
  (maximum             generic-function)
  (size                generic-function)
  (element-step        generic-function)
  (select              generic-function)
  (file-accept         generic-function)
  (url-src             generic-function)
  (alt-text            generic-function)
  (checkedp            generic-function)
  (input-mode-type     type)
  (input-mode          generic-function)
  (set-data-list       generic-function)
  (make-data-list      generic-function)
  (minimum-length      generic-function)
  (maximum-length      generic-function)

  "CLOG-Label - Class for CLOG Labels"
  (clog-label   class)
  (create-label generic-function)
  (label-for    generic-function)

  "CLOG-Select - Class for CLOG Selects"
  (clog-select         class)
  (create-select       generic-function)
  (clog-option         class)
  (create-option       generic-function)
  (clog-optgroup       class)
  (create-optgroup     generic-function)
  (selectedp           generic-function)
  (add-select-option   generic-function)
  (add-select-options  generic-function)
  (add-select-optgroup generic-function)

  "CLOG-Data-List - Class for CLOG Option Data Lists"
  (clog-data-list   class)
  (create-data-list generic-function)
  (add-option       generic-function)
  (add-options      generic-function)

  "CLOG-Text-Area - Class for CLOG Text Areas"
  (clog-text-area   class)
  (create-text-area generic-function)
  (word-wrap        generic-function)
  (columns          generic-function)
  (rows             generic-function)
  (disable-resize   generic-function))

(defsection @clog-canvas (:title "CLOG Canvas Objects")
  "CLOG-Canvas - Class for CLOG canvas objects"
  (clog-canvas   class)
  (create-canvas generic-function)

  (clog-context2d   class)
  (create-context2d generic-function)

  (clear-rect          generic-function)
  (fill-rect           generic-function)
  (stroke-rect         generic-function)
  (fill-text           generic-function)
  (stroke-text         generic-function)
  (line-width          generic-function)
  (line-cap            generic-function)
  (line-join           generic-function)
  (miter-limit         generic-function)
  (get-line-dash       generic-function)
  (set-line-dash       generic-function)
  (line-dash-offset    generic-function)
  (font-style          generic-function)
  (text-align-type     type)
  (text-align          generic-function)
  (text-baseline-type  type)
  (text-baseline       generic-function)
  (text-dir            generic-function)
  (fill-style          generic-function)
  (stroke-style        generic-function)
  (shadow-blur         generic-function)
  (shadow-color        generic-function)
  (shadow-offset-x     generic-function)
  (shadow-offset-y     generic-function)
  (begin-path          generic-function)
  (close-path          generic-function)
  (move-to             generic-function)
  (line-to             generic-function)
  (bezier-curve-to     generic-function)
  (quadratic-curve-to  generic-function)
  (arc                 generic-function)
  (arc-to              generic-function)
  (ellipse             generic-function)
  (rect                generic-function)
  (path-fill           generic-function)
  (path-stroke         generic-function)
  (path-clip           generic-function)
  (canvas-save         generic-function)
  (canvas-restore      generic-function))

(defsection @clog-multimedia (:title "CLOG Multimedia Objects")
  "CLOG-Multimedia - Base Class for CLOG multimedia objects"
  (clog-multimedia class)

  (loop-mediap       generic-function)
  (media-duration    generic-function)
  (media-source      generic-function)
  (media-position    generic-function)
  (mutedp            generic-function)
  (pausedp           generic-function)
  (seekingp          generic-function)
  (playback-ended-p  generic-function)
  (playback-rate     generic-function)
  (ready-to-play-p   generic-function)
  (media-volume      generic-function)
  (play-media        generic-function)
  (pause-media       generic-function)
  (load-media        generic-function)
  (can-play-type-p   generic-function)

  "CLOG-Multimedia - Event Handlers

    The standard event order for a normal file load is:
      On_Load_Start
      On_Duration_Change
      On_Loaded_Meta_Data
      On_Loaded_Data
      On_Progress
      On_Can_Play
      On_Can_Play_Though"

  (set-on-media-abort      generic-function)
  (set-on-media-error      generic-function)
  (set-on-can-play         generic-function)
  (set-on-can-play-through generic-function)
  (set-on-duration-change  generic-function)
  (set-on-emptied          generic-function)
  (set-on-ended            generic-function)
  (set-on-loaded-data      generic-function)
  (set-on-loaded-meta-data generic-function)
  (set-on-load-start       generic-function)
  (set-on-play             generic-function)
  (set-on-pause            generic-function)
  (set-on-playing          generic-function)
  (set-on-progress         generic-function)
  (set-on-rate-change      generic-function)
  (set-on-seeked           generic-function)
  (set-on-seeking          generic-function)
  (set-on-stalled          generic-function)
  (set-on-suspend          generic-function)
  (set-on-time-update      generic-function)
  (set-on-volume-change    generic-function)
  (set-on-waiting          generic-function)

  "Clog-Audio - Class for CLOG Audio Control"
  (clog-audio     class)
  (create-audio   generic-function)

  "Clog-Video - Class for CLOG Video Control"
  (clog-video     class)
  (create-video   generic-function))

(defsection @clog-body (:title "CLOG Body Objects")
  "CLOG-Body - CLOG Body Objects"
  (clog-body class)

  "CLOG-Body - Properties"
  (run               generic-function)
  (set-html-on-close generic-function)
  (window            generic-function)
  (html-document     generic-function)
  (location          generic-function)
  (navigator         generic-function))

(defsection @clog-window (:title "CLOG Window Objects")
  "CLOG-Window - CLOG Window Objects"
  (clog-window class)

  "CLOG-Window - Properties"
  (window-name             generic-function)
  (inner-height            generic-function)
  (inner-width             generic-function)
  (outer-height            generic-function)
  (outer-width             generic-function)
  (x-offset                generic-function)
  (y-offset                generic-function)
  (left                    generic-function)
  (top                     generic-function)
  (pixel-ratio             generic-function)
  (screen-width            generic-function)
  (screen-height           generic-function)
  (screen-available-width  generic-function)
  (screen-available-height generic-function)
  (screen-available-top    generic-function)
  (screen-available-left   generic-function)
  (screen-color-depth      generic-function)

  "CLOG-Window - Methods"
  (alert            generic-function)
  (log-console      generic-function)
  (log-error        generic-function)
  (print-window     generic-function)
  (scroll-by        generic-function)
  (scroll-to        generic-function)
  (open-window      generic-function)
  (close-window     generic-function)
  (close-connection generic-function)
  (storage-type     type)
  (storage-length   generic-function)
  (storage-key      generic-function)
  (storage-remove   generic-function)
  (storage-element  generic-function)

  "CLOG-Window - Events"
  (set-on-abort              generic-function)
  (set-on-error              generic-function)
  (set-on-before-unload      generic-function)
  (set-on-hash-change        generic-function)
  (set-on-orientation-change generic-function)
  (set-on-storage            generic-function)
  (move-window-by            generic-function)
  (move-window-to            generic-function)
  (resize-by                 generic-function)
  (resize-to                 generic-function))

(defsection @clog-document (:title "CLOG Document Objects")
  "CLOG-Document - CLOG Document Objects"
  (clog-document class)

  (domain           generic-function)
  (input-encoding   generic-function)
  (last-modified    generic-function)
  (referer          generic-function)
  (title            generic-function)
  (document-url     generic-function)
  (head-element     generic-function)
  (body-element     generic-function)
  (document-element generic-function)
  (ready-state      generic-function)
  (load-css         generic-function)
  (load-script      generic-function)
  (put              generic-function)
  (put-line         generic-function)
  (put-br           generic-function)
  (new-line         generic-function)

  "CLOG-Document - Events"
  (set-on-full-screen-change generic-function))

(defsection @clog-navigator (:title "CLOG Navigator Objects")
  "CLOG-Navigator - CLOG Navigator Objects"
  (clog-navigator class)

  "CLOG-Navigator - Properties"
  (cookie-enabled-p generic-function)
  (language         generic-function)
  (user-agent       generic-function)
  (vendor           generic-function))

(defsection @clog-location (:title "CLOG Location Objects")
  "Clog-Location - CLOG Location Objects"
  (clog-location class)

  "CLOG-Location - Properties"
  (url         generic-function)
  (hash        generic-function)
  (host        generic-function)
  (host-name   generic-function)
  (origin      generic-function)
  (path-name   generic-function)
  (port        generic-function)
  (protocol    generic-function)
  (url-search  generic-function)

  "CLOG-Location - Methods"
  (reload      generic-function)
  (url-replace generic-function)
  (url-assign  generic-function))

(defsection @clog-helpers (:title "CLOG Helper Functions")
  "Tutorial and demo helpers"
  (clog-install-dir  function)
  (open-manual       function)
  (run-tutorial      function)
  (load-tutorial     function)
  (run-demo          function)
  (load-demo         function)
  (clog-repl         function)
  (save-body-to-file function)

  "Functions for Compilation and Documentation"
  ;; contained in clog-docs.lisp
  (load-world       function)
  (make-mark-down   function)
  (make-html        function)
  (make-world       function))

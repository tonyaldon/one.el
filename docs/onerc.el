;; We do this because we want org-mode links in source block
;; (with org-mode content) to be visible (link and description parts).
;; You can use (remove-hook 'org-mode-hook #'visible-mode) to remove it.
(add-hook 'org-mode-hook #'visible-mode)

(require 'ol)

(defun one-ox-link-youtube (link description type info)
  "Export youtube link `[[youtube:youtube-id]]'."
  (let ((youtube-embed-link (concat "https://www.youtube.com/embed/" link)))
    (jack-html
     `(:iframe
       (@
        :style "width:100%;aspect-ratio: 16 / 9;"
        :src ,(concat youtube-embed-link "?"
                      ;; "autoplay=1"
                      "&fs=1"      ; show the fullscreen button
                      "&modestbranding=1" ; don't show YouTube logo in bottom right
                      "&rel=0" ; show only related video from my channel
                      "&widget_referrer=https://one.tonyaldon.com" ; analytics
                      )
        :title "YouTube video player playing "
        :frameborder "0"
        :allow "fullscreen;accelerometer;autoplay;clipboard-write;encrypted-media;gyroscope;picture-in-picture;web-share")))))

(org-link-set-parameters "youtube" :export #'one-ox-link-youtube)

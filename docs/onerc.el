;; We do this because we want org-mode links in source block
;; (with org-mode content) to be visible (link and description parts).
;; You can use (remove-hook 'org-mode-hook #'visible-mode) to remove it.
(add-hook 'org-mode-hook #'visible-mode)

;; Put BSD specific configuration here

;; BSD ls doesn't support '--group-directories-first' from the general
;; configuration so override the switches to exclude it.
(setq dired-listing-switches "-aGBhlp")

(provide 'bsd)
;;; bsd.el ends here

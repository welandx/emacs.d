
(setq default-frame-alist '((width . 300)
			    (height . 240)
			    (alpha-background . 80)))


(setq-default mode-line-format
	      (list
	       " "
	       'mode-line-mule-info
	       " "
	       'mode-line-modified
	       " "
	       'mode-line-buffer-identification
	       " "
	       'mode-line-position
	       " "
	       'mode-line-misc-info
	       " "
	       'mode-line-end-spaces
	       ))

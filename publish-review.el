(require 'use-package)
(setq use-package-verbose t)
(setq build-directory (file-name-as-directory (or (getenv "TMPDIR") "/tmp/publish-review-build/")))
(setq user-emacs-directory build-directory)
(setq publish-url (or (getenv "PUBLISH_URL") "somewhere"))
(use-package org
  :demand t
  :init
  (require 'ox-publish)
  (setq org-publish-use-timestamps-flag nil)
  (setq org-publish-timestamp-directory build-directory)
  (setq org-export-timestamp-file nil)
  (setq org-directory (or (file-name-as-directory (getenv "ORGDIR")) build-directory))
  (setq org-bibtex-file  (file-name-concat org-directory "bib" "bibliography.bib"))
  (setq org-cite-global-bibliography (list org-bibtex-file)))

(use-package ox-rss
  :ensure t)

(use-package org-id
  :demand t
  :init
  ;; (setq org-id-locations-file (file-name-concat org-directory ".org-id-locations"))
  ;; (setq org-id-locations-file-relative t)
  :config
  (org-id-update-id-locations))

(use-package org-roam
  :ensure t
  :demand t
  :after org-id
  :init
  (setq org-roam-directory org-directory)
  (setq org-roam-mode-sections
	(list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section
              ))
  ;; (setq org-roam-db-location (file-name-concat org-directory "org-roam.db"))
  (setq org-roam-verbose t)
  :config
  (org-roam-update-org-id-locations)
  (org-roam-db-sync))

(use-package org-roam-bibtex
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package citar
  :ensure t
  :init
  (setq citar-bibliography (list org-bibtex-file)))

(defun review-list-backlinks (node-id)
  (let ((backlinks (mapcar
		    (lambda (n)
		      (org-element-create
		       'item '(:bullet "- " :pre-blank 0)
		       (org-element-create
			'link
			`(:type "id" :type-explicit-p t :path ,(car n) :format "bracket")
			(cadr n))))
		    (org-roam-db-query
		     [:select [nodes:id nodes:title]
			      :from links
			      :join nodes
			      :on (= links:source nodes:id)
			      :where (= dest $s1)
			      :and (= type "id")
			      ]
		     node-id))))
    (org-element-create 'headline '(:title "Backlinks" :level 1 :raw-value "Backlinks")
			(org-element-create 'plain-list '(:type unordered)
					    backlinks))))

(defun review-filter-parse-tree (tree _backend info)
  (if (plist-get info :with-backlinks)
      (org-element-adopt tree (review-list-backlinks (org-element-property :ID tree))))
  tree)

(setq org-export-filter-parse-tree-functions '(review-filter-parse-tree))

(defun drawer-function (name contents)
  (and (org-string-nw-p contents)
       (format "<pre class=\"%s\">\n%s</pre>" 
	       "example"
	       contents
	       ;; (concat "<table>\n" (mapconcat (lambda (x)
	       ;; 					(let*
	       ;; 					    ((prop (s-match org-property-re x))
	       ;; 					     (key (-third-item prop))
	       ;; 					     (value (-fourth-item prop))
	       ;; 					     (parsed-value (org-element-parse-secondary-string value '(link paragraph timestamp citation))))
	       ;; 					  (format "<tr><th> %s </th><td> %s </td></tr> \n" key value)))
	       ;; 				      (split-string contents "\n"))
	       ;; 	       "</table>")
	       )))

(org-export-define-derived-backend 'review-html 'html
  :translate-alist '((property-drawer . org-html-drawer)))

(defun org-review-publish-to-html (plist filename pub-dir)
     (org-publish-org-to 'review-html filename
			 (concat (when (> (length org-html-extension) 0) ".")
				 (or (plist-get plist :html-extension)
				     org-html-extension
				     "html"))
			 plist pub-dir))

(defun review-citation-export-citation (citation _style backend info)
  "Export CITATION object.
STYLE is the expected citation style, as a pair of strings or nil.  INFO is the
export communication channel, as a property list."
  (let* ((citations-as-org-links
	  (org-cite-mapconcat
	   (lambda (ref)
	     (org-element-parse-secondary-string
	      (let ((links (org-roam-db-query [:select [node-id] :from refs
						       :where (= ref $s1)]
					      ref)))
		(if links (format "[[id:%s][%s]]"
				  (caar links)
				  ref))
		ref)
	      '(link)))
	   (org-cite-get-references citation t)
	   ", ")))
    citations-as-org-links))

(org-cite-register-processor 'review-citation-processor
  :export-citation #'review-citation-export-citation)

(setq org-cite-export-processors '((t review-citation-processor)))

(setq itihas-review-publish-project-alist `(("html-export"
					     :base-directory ,(file-name-concat org-directory "public")
					     :publishing-directory ,(file-name-concat org-directory "out")
					     :base-extension "org"
					     :publishing-function org-review-publish-to-html
					     :html-format-drawer-function drawer-function
					     :with-toc nil
					     :with-broken-links t
					     :with-backlinks t
					     :with-latex t
					     :with-properties t
					     :html-html5-fancy t
					     :prefer-user-labels t
					     :auto-sitemap t
					     :sitemap-filename "archive.org"
					     :sitemap-style list
					     :sitemap-title "Archive"
					     :html-head-extra "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>"
					     )
					    ("images"
					     :base-directory ,(file-name-concat org-directory "public/images/")
					     :publishing-directory ,(file-name-concat org-directory "out/images")
					     :base-extension "jpg\\|jpeg\\|gif\\|png"
					     :recursive t
					     :publishing-function org-publish-attachment)
					    ("rss-reviewed"
					     :base-directory ,(file-name-concat org-directory "public")
					     :base-extension "org"
					     :rss-image-url ,(file-name-concat (file-name-as-directory publish-url) "images" "home.jpg")
					     :html-link-home ,publish-url
					     :html-link-use-abs-url t
					     :rss-extension "xml"
					     :publishing-directory ,(file-name-concat org-directory "out")
					     :publishing-function (org-rss-publish-to-rss)
					     :section-numbers nil
					     :exclude ".*"            ;; To exclude all files...
					     :include ("reviewed-feed.org")   ;; ... except reviewed.org.
					     :table-of-contents nil)))

(defun publish-itihas-review ()
  (interactive)
  (org-publish-projects itihas-review-publish-project-alist))

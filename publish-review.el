;; PRELUDE
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
  (setq org-id-link-to-org-use-id create-if-interactive)
  (setq org-link-link-consider-parent-id t)
  (setq org-id-link-use-context t)
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

;; BACKLINKS
(defun create-backlinks-list (node-id)
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

(defun parse-tree-add-backlinks (tree _backend info)
  (if (plist-get info :with-backlinks)
      (org-element-adopt tree (create-backlinks-list (org-element-property :ID tree))))
  tree)

;; ANCHOR IDS
(defun sluggify (str)
  (replace-regexp-in-string
   "[^a-z0-9-]" ""
   (mapconcat 'identity
	      (cl-remove-if-not 'identity
                             (seq-take (split-string
					(downcase str) " ")
				       6))
	      "-")))

(defun parse-tree-custom-ids (tree _backend info)
  """adds the `CUSTOM_ID` attribute to all headlines and inlinetasks in org, so that when `org-html--reference` is called, it has an existing ID to use instead of generating a new one every time. The anchor has to be globally unique, so what this does to try to make it that + deterministic without polluting my actual notebook with persistent IDs for _every last headline_ is similar to the `org-id-link-use-context` approach, i.e. <org-id>-<headline-slug>."""
  (org-element-map tree org-element-all-elements
    (lambda (n)
      (if (org-element-type-p n '(headline
				  inlinetask
				  quote-block
				  example-block
				  inline-src-block
				  paragraph))
	  (progn (org-element-put-property
		  n :CUSTOM_ID
		  (sluggify (concat (org-element-property-inherited :ID n) " "
				    (org-element-property :raw-value n))))
		 nil)))))

;; PROPERTY DRAWER PARSING
;; Current approach to this is to change how the drawer itself is parsed, and use a drawer parsing funciton to parse the property drawer by creating a derived backend. I think I'm better off instead creating another parse tree filter to look at the properties and create the HTML elements I want more directly -- in some cases as snippets, but notably I want to turn citekeys that appear in `ROAM_REFS` into references, for which citar is probably best.
(defun parse-tree-property-drawers (tree _backend info)
  (org-element-map tree org-element-all-elements)
  (lambda (n)
    (if (org-element-type-p n '(property-drawer))
	(org-element-create 'table '()))))


;; PARSE TREE FILTERS
(setq org-export-filter-parse-tree-functions '(parse-tree-add-backlinks parse-tree-custom-ids))


;; TAGS




;; CITATION PARSING
;; Registers a citation processor that creates links to notes if I have them. I want it to create links to references that I dump in a generated references section at the bottom of the page if I don't have them. The citation processor needs to cause changes to the parse tree, and I don't know what order they evaluate in, so implementation thereof will be a little bit interesting.
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


;; PUBLISH PROJECT ALIST
(setq itihas-review-publish-project-alist `(("html-export"
					     :base-directory ,(file-name-concat org-directory "public")
					     :publishing-directory ,(file-name-concat org-directory "out")
					     :base-extension "org"
					     :publishing-function org-html-publish-to-html
					     :with-author nil
					     :with-toc nil
					     :section-numbers nil
					     :with-broken-links t
					     :with-backlinks t
					     :with-latex t
					     :with-properties '("ROAM_REF" "MTIMES")
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

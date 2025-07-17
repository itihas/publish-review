(require 'ox-publish)
(require 'org-roam)
(setq org-directory "./")
(setq org-roam-directory org-directory)
(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            #'org-roam-unlinked-references-section
            ))
(setq org-publish-use-timestamps-flag nil)
(setq org-publish-timestamp-directory "$TMPDIR")
(setq org-roam-db-location "./org-roam.db")
(setq org-id-locations-file "./org-id-locations")
(setq org-bibtex-file  (file-name-concat org-directory "bib" "bibliography.bib"))
(setq org-cite-global-bibliography (list org-bibtex-file))
(setq citar-bibliography (list org-bibtex-file))

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
    (if backlinks
	(org-element-create 'headline '(:title "Backlinks" :level 1 :raw-value "Backlinks")
			    (org-element-create 'plain-list '(:type unordered)
						backlinks))
      (org-element-create 'paragraph nil "This node is a singleton!"))))

(defun review-filter-parse-tree (tree _backend info)
  (if (plist-get info :with-backlinks)
      (org-element-adopt tree (review-list-backlinks (org-element-property :ID tree))))
  tree)

(setq org-export-filter-parse-tree-functions '(review-filter-parse-tree))

(defun review-filter-property-drawer (contents _backend info)
  (if (plist-get info :fancy-property-drawer)
      (and (org-string-nw-p contents)
	   (format "<div class=\"%s\">\n%s</div>"
		   (plist-get info :html-content-class) contents))))

(setq org-export-filter-property-drawer-functions '(review-filter-property-drawer))


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
				   :publishing-function org-html-publish-to-html
				   :with-toc nil
				   :with-broken-links t
				   :with-backlinks t
				   :with-latex t
				   :with-properties t
				   :fancy-property-drawer t
				   :html-html5-fancy t
				   :auto-sitemap t
				   :sitemap-filename "archive.org"
				   :sitemap-style list
				   :sitemap-title "Archive"
				   :html-head-extra "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>"
				   )
				  ("images"
				   :base-directory ,(file-name-concat org-directory "public/images/")
				   :publishing-directory ,(file-name-concat org-directory "out/images")
				   :base-extension "jpg\\|gif\\|png"
				   :recursive t
				   :publishing-function org-publish-attachment)))

(defun publish-itihas-review ()
  (org-id-update-id-locations (org-roam-list-files))
  (org-publish-projects itihas-review-publish-project-alist))

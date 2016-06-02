;;
;; https://github.com/syohex/emacs-maven-search/blob/master/maven-search.el
;;
;; (require 'json)
;; (require 'request)

;; (request
;;  "https://search.maven.org/solrsearch/select?"

;; (defgroup maven-search nil
;;   "Search maven artifacts."
;;   :group 'maven)

;; (defgroup maven-search-rows 20
;;   "Number of list to shows"
;;   :group 'maven)

;; (defsubst maven-search--construct-url (query)
;;   (concat "https://search.maven.org/solrsearch/select?" query))


;; ;;;###autoload
;; (defun maven-search (keyword)
;;   (interactive
;; 	(list (read-string "Search artifact: " nil 'maven-search--history)))
;;   (unless (string= keyword)
;; 	 (error "Error: keyword must be string."))
;;   (when (string= keyword "")
;; 	 (error "Error: keyword is empty string"))
;;   (maven-search--search-artifact keyword))

;; (provide 'maven-search)

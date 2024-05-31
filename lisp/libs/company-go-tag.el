;;; company-go-tag.el --- company backend for go tag

;;; Commentary:
;; 


;;; Code:


(require 'cl-lib)
(require 'company)
(require 'treesit)

(defcustom company-go-tag-alist
  '(("json" . ("omitempty"
	       "string"))
    ("gorm" . ("column" "type" "serializer" "size" "primaryKey"
			     "unique" "default" "precision" "scale" "not null"
			     "autoIncrement" "autoIncrementIncrement" "embedded" "embeddedPrefix"
			     "autoCreateTime" "autoUpdateTime"
			     "index" "uniqueIndex" "check"
			     "-" "<-" "->"))
    ;; https://github.com/go-playground/validator?tab=readme-ov-file
    ("validate" . ("required"
		   ;;Fields
		   "eqcsfield" "eqfield" "fieldcontains" "fieldexcludes" "gtcsfield" "gtecsfield" "gtefield" "gtfield" "ltcsfield" "ltecsfield" "ltefield" "ltfield" "necsfield" "nefield"
		   ;;Network
		   "cidr" "cidrv4" "cidrv6" "datauri" "fqdn" "hostname" "hostname_port" "hostname_rfc1123" "ip" "ip4_addr" "ip6_addr" "ip_addr" "ipv4" "ipv6" "mac" "tcp4_addr" "tcp6_addr" "tcp_addr" "udp4_addr" "udp6_addr" "udp_addr" "unix_addr" "uri" "url" "http_url" "url_encoded" "urn_rfc2141"

		   ;;Strings
		   "alpha" "alphanum" "alphanumunicode" "alphaunicode" "ascii" "boolean" "contains" "containsany" "containsrune" "endsnotwith" "endswith" "excludes" "excludesall" "excludesrune" "lowercase" "multibyte" "number" "numeric" "printascii" "startsnotwith" "startswith" "uppercase"

		   ;;Format
		   "base64" "base64url" "base64rawurl" "bic" "bcp47_language_tag" "btc_addr" "btc_addr_bech32" "credit_card" "mongodb" "cron" "spicedb" "datetime" "e164" "email" "eth_addr" "hexadecimal" "hexcolor" "hsl" "hsla" "html" "html_encoded" "isbn" "isbn10" "isbn13" "issn" "iso3166_1_alpha2" "iso3166_1_alpha3" "iso3166_1_alpha_numeric" "iso3166_2" "iso4217" "json" "jwt" "latitude" "longitude" "luhn_checksum" "postcode_iso3166_alpha2" "postcode_iso3166_alpha2_field" "rgb" "rgba" "ssn" "timezone" "uuid" "uuid3" "uuid3_rfc4122" "uuid4" "uuid4_rfc4122" "uuid5" "uuid5_rfc4122" "uuid_rfc4122" "md4" "md5" "sha256" "sha384" "sha512" "ripemd128" "ripemd128" "tiger128" "tiger160" "tiger192" "semver" "ulid" "cve"

		   ;;Comparisons
		   "eq" "eq_ignore_case" "gt" "gte" "lt" "lte" "ne" "ne_ignore_case"

		   ;;Other
		   "dir" "dirpath" "file" "filepath" "image" "isdefault" "len" "max" "min" "oneof" "required" "required_if" "required_unless" "required_with" "required_with_all" "required_without" "required_without_all" "excluded_if" "excluded_unless" "excluded_with" "excluded_with_all" "excluded_without" "excluded_without_all" "unique"
		   ))
    ("form"))
  "Company go tag alist."
  :type 'list
  :group 'company-go-tag)

(defun company-go-tag-field-name ()
  (let* ((node (treesit-node-at (point)))
	 (node-type (treesit-node-type node))
	 (node-parent (treesit-node-parent node))
	 (node-parent-type (treesit-node-type node-parent)))
    (if (string-equal "field_declaration" node-parent-type)
	(let ((identifier (treesit-node-child node-parent 0)))
	  (treesit-node-text identifier)))))


(defun company-go-tag--prefix ()
  "Check if is company prefix."
  (if (eq major-mode 'go-ts-mode)
      (let* ((node (treesit-node-at (point)))
	     (node-type (treesit-node-type node))
	     (node-parent (treesit-node-parent node))
	     (node-parent-type (treesit-node-type node-parent)))
	(and (string-equal "raw_string_literal" node-type)
	     (string-equal "field_declaration" node-parent-type)
	     (company-grab-symbol)))))

(defun company-go-tag--candidates (prefix)
  (let* ((node (treesit-node-at (point)))
	 (k (when (save-excursion
		    (re-search-backward "[` ,\"]\\(.+\\):?\""
		     (treesit-node-start node) t 1))
	      (match-string 1)))
	 (scope (when k (string-replace ":" "" k)))
	 (candidates (cdr (assoc scope company-go-tag-alist))))
    (cl-remove-if-not (lambda (c) (string-prefix-p prefix c))
		      (if (not (string-suffix-p ":" k))
			  (mapcar #'car company-go-tag-alist)
			;; TODO support function
			candidates))))


;;;###autoload
(defun company-go-tag (command &optional arg &rest ignored)
  "Companybackend for protobuf."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-go-tag))
    (prefix (company-go-tag--prefix))
    (candidates (company-go-tag--candidates arg))))


(provide 'company-go-tag)

;;; company-go-tag.el ends here

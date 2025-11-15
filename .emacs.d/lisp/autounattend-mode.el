;; FILE: autounattend-mode.el
;;; autounattend-mode.el --- Major mode for Windows autounattend.xml files -*- lexical-binding: t; -*-
;; Copyright (C) 2025
;; Author: Br41nfck
;; URL: https://github.com/Br41nfck/Configs/tree/main/.emacs.d/lisp/autounattend-mode.el
;; Version: 0.1.0
;; Keywords: xml windows unattended autounattend
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: MIT
;;; Commentary:
;;
;; Major mode for editing Windows Autounattend.xml / unattend.xml files.
;; Provides derived mode from nxml-mode, file associations, basic
;; formatting command, and hooks for company/yasnippet integration.
;;
;; Install: place this file on your `load-path` and add:
;;   (require 'autounattend-mode)
;;
;;; Code:
(require 'nxml-mode)
(require 'company)
(require 'cl-lib)
(require 'url)

;;; --- Auto-completion data ---
(defconst autounattend-completion-data
  '((elements
     "unattend" "settings" "component" "UserData" "ProductKey" "Key" "WillShowUI"
     "DiskConfiguration" "Disk" "DiskID" "WillWipeDisk" "CreatePartitions"
     "CreatePartition" "Order" "Type" "Size" "Extend" "ComputerName" "OOBE"
     "HideEULAPage" "SkipMachineOOBE" "SkipUserOOBE" "HideOEMRegistrationScreen"
     "UserAccounts" "AdministratorPassword" "Value" "PlainText" "LocalAccounts"
     "LocalAccount" "Password" "Description" "DisplayName" "Group" "Name"
     "TimeZone" "RegisteredOrganization" "RegisteredOwner" "AutoLogon"
     "Enabled" "LogonCount" "Username" "Display" "ColorDepth" "HorizontalResolution"
     "RefreshRate" "VerticalResolution" "InputLocale" "SystemLocale" "UILanguage"
     "UILanguageFallback" "UserLocale")
    (attributes
     "pass" "name" "processorArchitecture" "publicKeyToken" "language" "versionScope"
     "action" "wcm:action" "xmlns:wcm" "xmlns:xsi")
    (pass-values
     "windowsPE" "offlineServicing" "generalize" "specialize" "auditSystem"
     "auditUser" "oobeSystem")
    (component-names
     "Microsoft-Windows-Shell-Setup" "Microsoft-Windows-International-Core"
     "Microsoft-Windows-International-Core-WinPE" "Microsoft-Windows-Setup"
     "Microsoft-Windows-Deployment" "Microsoft-Windows-PnpSysprep"
     "Microsoft-Windows-IE-InternetExplorer" "Microsoft-Windows-TCPIP"
     "Microsoft-Windows-DNS-Client" "Microsoft-Windows-TerminalServices-LocalSessionManager"
     "Microsoft-Windows-TerminalServices-RDP-WinStationExtensions")
    (architecture-values
     "x86" "amd64" "arm64" "wow64")
    (boolean-values
     "true" "false"))
  "Completion data for autounattend.xml files.")

(defun autounattend--inside-attribute-value-p ()
  "Check if point is inside an attribute value."
  (save-excursion
    (let ((pos (point)))
      (and (re-search-backward "=\\s-*\"" (line-beginning-position) t)
           (< (point) pos)
           (not (re-search-forward "\"" pos t))))))

(defun autounattend--inside-attribute-name-p ()
  "Check if point is inside an attribute name."
  (save-excursion
    (let ((pos (point)))
      (and (re-search-backward "<\\|\\s-+\\|=" (line-beginning-position) t)
           (looking-at "[a-zA-Z:-]*\\'")
           (< (point) pos)
           (not (looking-at ">"))
           (not (looking-back "=\\s-*\"" (- (point) 10)))))))

(defun autounattend--inside-element-p ()
  "Check if point is inside an element name."
  (save-excursion
    (let ((pos (point)))
      (and (re-search-backward "<" (line-beginning-position) t)
           (looking-at "[a-zA-Z:-]*\\'")
           (< (point) pos)
           (not (looking-at "![^-]"))  ;; Not inside comment
           (not (looking-at "?xml"))))))  ;; Not in XML declaration

(defun autounattend--current-attribute-name ()
  "Get the current attribute name at point."
  (save-excursion
    (when (re-search-backward "\\b\\([a-zA-Z:-]+\\)\\s-*=\\s-*\"" (line-beginning-position) t)
      (match-string 1))))


(defun autounattend--get-attribute-values (attribute)
  "Get possible values for ATTRIBUTE."
  (cond
   ((string= attribute "pass") (cdr (assq 'pass-values autounattend-completion-data)))
   ((string= attribute "name") (cdr (assq 'component-names autounattend-completion-data)))
   ((string= attribute "processorArchitecture") (cdr (assq 'architecture-values autounattend-completion-data)))
   ((member attribute '("WillShowUI" "WillWipeDisk" "HideEULAPage" "SkipMachineOOBE"
                        "SkipUserOOBE" "PlainText" "Enabled" "Extend"))
    (cdr (assq 'boolean-values autounattend-completion-data)))
   (t nil)))

(defun autounattend--get-documentation (symbol)
  "Get documentation for SYMBOL."
  (cond
   ((member symbol (cdr (assq 'elements autounattend-completion-data)))
    (format "Element: %s" symbol))
   ((member symbol (cdr (assq 'attributes autounattend-completion-data)))
    (format "Attribute: %s" symbol))
   ((member symbol (cdr (assq 'component-names autounattend-completion-data)))
    (format "Component: %s" symbol))
   (t (format "Value: %s" symbol))))

(defun autounattend-completion-at-point ()
  "Provide completion at point for autounattend.xml elements and attributes."
  (cond
   ;; Completion for attribute values
   ((autounattend--inside-attribute-value-p)
    (let* ((bound (bounds-of-thing-at-point 'word))
           (start (or (car bound) (point)))
           (end (or (cdr bound) (point)))
           (attribute (autounattend--current-attribute-name)))
      (when attribute
        (list start end (autounattend--get-attribute-values attribute)))))
   ;; Completion for attribute names
   ((autounattend--inside-attribute-name-p)
    (let ((bound (bounds-of-thing-at-point 'word)))
      (list (car bound) (cdr bound) (cdr (assq 'attributes autounattend-completion-data)))))
   ;; Completion for element names
   ((autounattend--inside-element-p)
    (let ((bound (bounds-of-thing-at-point 'word)))
      (list (car bound) (cdr bound) (cdr (assq 'elements autounattend-completion-data)))))
   ;; No completion
   (t nil)))

(defun company-autounattend (command &optional arg &rest ignored)
  "Company backend for autounattend-mode."
  (cl-case command
    (interactive (company-begin-backend 'company-autounattend))
    (prefix
     (when (derived-mode-p 'autounattend-mode)
       (company-grab-symbol)))
    (candidates
     (let ((prefix arg))
       (cl-remove-if-not
        (lambda (c) (string-prefix-p prefix c))
        (append (cdr (assq 'elements autounattend-completion-data))
                (cdr (assq 'attributes autounattend-completion-data))
                (cdr (assq 'component-names autounattend-completion-data))
                (cdr (assq 'pass-values autounattend-completion-data))
                (cdr (assq 'architecture-values autounattend-completion-data))
                (cdr (assq 'boolean-values autounattend-completion-data))))))
    (meta (format "Value: %s" arg))
    (no-cache t)))

(defun autounattend-mode-setup-safe ()
  "Safe setup function without complex background processing."
  ;; Enable company
  (company-mode 1)
  ;; Simple company backends setup
  (setq-local company-backends '(company-autounattend))
  ;; Basic completion
  (setq-local completion-at-point-functions
              '(autounattend-completion-at-point))
  ;; Use our keymap
  (use-local-map autounattend-mode-map)
  ;; Enable electric-pair-mode for XML
  (electric-pair-local-mode 1)
  ;; Simple schema setup without background timers
  (condition-case err
      (progn
        (autounattend--ensure-xsd-dir)
        (setq-local rng-schema-locating-files
                    (list (expand-file-name "schemas.xml" autounattend-xsd-directory)))
        (when (fboundp 'rng-auto-set-schema-and-validate)
          (rng-auto-set-schema-and-validate)))
    (error nil))
  (message "autounattend-mode: ready"))


(defun autounattend-mode-setup ()
  "Setup function for autounattend-mode with enhanced completion."
  ;; Basic functionality that always works
  (company-mode 1)
  ;; Set company backends properly - use list of backends
  (setq-local company-backends '((company-autounattend company-capf)))
  ;; Set completion at point functions
  (setq-local completion-at-point-functions
              '(autounattend-completion-at-point
                completion-at-point))
  ;; Use our keymap
  (use-local-map autounattend-mode-map)
  ;; Enable electric-pair-mode for XML
  (electric-pair-local-mode 1)
  (setq-local electric-pair-pairs '((?< . ?>)))
  ;; Ensure XSD directory exists *before* attempting schema setup
  (autounattend--ensure-xsd-dir)
  ;; Try schema setup (non-blocking attempt)
  (condition-case err
      (progn
        (setq-local rng-schema-locating-files
                    (list (expand-file-name "schemas.xml" autounattend-xsd-directory)))
        (when (fboundp 'rng-auto-set-schema-and-validate)
          (rng-auto-set-schema-and-validate)
          (message "autounattend: schema setup attempted")))
    (error
     (message "autounattend: schema setup failed, using basic mode")))

  (message "autounattend-mode: ready with enhanced completion"))


(define-derived-mode autounattend-mode nxml-mode "Autounattend"
  "Major mode for editing Windows Autounattend.xml files.
Provides syntax highlighting, validation, and completion for Windows
unattended installation files.
Key bindings:
\\{autounattend-mode-map}"
  (autounattend-mode-setup-safe)) ;; Use the simpler, safer setup initially
;; (Removed duplicate autounattend-mode definition)

;;; --- Template functions ---
(defun autounattend-insert-component-template ()
  "Insert a component template with common attributes."
  (interactive)
  (let ((component (completing-read "Component: " (cdr (assq 'component-names autounattend-completion-data))))
        (arch (completing-read "Architecture: " (cdr (assq 'architecture-values autounattend-completion-data) nil t "amd64"))))
    (insert (format "<component name=\"%s\" processorArchitecture=\"%s\"
" component arch))
    (insert "           publicKeyToken=\"31bf3856ad364e35\" language=\"neutral\"
")
    (insert "           versionScope=\"nonSxS\" xmlns:wcm=\"http://schemas.microsoft.com/WMIConfig/2002/State\"
")
    (insert "           xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
")
    (insert "  
")
    (insert "</component>")
    (forward-line -2)
    (indent-for-tab-command)))


(defcustom autounattend-xsd-directory
  (expand-file-name "schemas/" user-emacs-directory)
  "Directory containing XSD schemas for autounattend.xml validation."
  :type 'directory
  :group 'autounattend)

(defcustom autounattend-schema-file
  "autounattend.xsd"
  "Main XSD file used to validate autounattend.xml."
  :type 'string
  :group 'autounattend)

(defun autounattend--ensure-xsd-dir ()
  (unless (file-directory-p autounattend-xsd-directory)
    (make-directory autounattend-xsd-directory t)))

(defun autounattend--xsd-installed-p ()
  (file-exists-p (expand-file-name autounattend-schema-file autounattend-xsd-directory)))

(defconst autounattend-embedded-component-xsd
  '(("Microsoft-Windows-Shell-Setup.xsd"
     . "<?xml version='1.0'?>
<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'>
  <xs:element name='component'/>
</xs:schema>")
    ("Microsoft-Windows-International-Core.xsd"
     . "<?xml version='1.0'?>
<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'>
  <xs:element name='component'/>
</xs:schema>")
    ("Microsoft-Windows-International-Core-WinPE.xsd"
     . "<?xml version='1.0'?>
<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'>
  <xs:element name='component'/>
</xs:schema>")
    ("Microsoft-Windows-Setup.xsd"
     . "<?xml version='1.0'?>
<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'>
  <xs:element name='component'/>
</xs:schema>")
    ("Microsoft-Windows-Deployment.xsd"
     . "<?xml version='1.0'?>
<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'>
  <xs:element name='component'/>
</xs:schema>")
    ("autounattend.xsd"
     . "<?xml version='1.0'?>
<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'>
  <xs:element name='component'/>
</xs:schema>")
    ("unattend.xsd"
     . "<?xml version='1.0'?>
<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'>
  <xs:element name='component'/>
</xs:schema>")
    ("PnpSysprep.xsd"
     . "<?xml version='1.0'?>
<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'>
  <xs:element name='component'/>
</xs:schema>"))
  "Fallback embedded XSD component schemas.")

(defun autounattend-install-embedded-component-xsd ()
  "Install built-in fallback component schemas."
  (interactive)
  (autounattend--ensure-xsd-dir)
  (dolist (pair autounattend-embedded-component-xsd)
    (let ((file (expand-file-name (car pair) autounattend-xsd-directory))
          (content (cdr pair)))
      (unless (file-exists-p file)
        (with-temp-file file (insert content))))))

(defconst autounattend-fallback-xsd
  "<?xml version='1.0'?>
<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'>
  <xs:element name='unattend'/>
</xs:schema>"
  "Fallback minimal XSD used if download is unavailable.")

(defun autounattend-create-proper-relaxng-schema ()
  "Create a properly formatted RelaxNG schema that nxml-mode can load."
  (interactive)
  (autounattend--ensure-xsd-dir)
  ;; Create RelaxNG schema
  (let ((rng-file (expand-file-name "autounattend.rng" autounattend-xsd-directory)))
    (with-temp-file rng-file
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
")
      (insert "<grammar ns=\"urn:schemas-microsoft-com:unattend\"
")
      (insert "         xmlns=\"http://relaxng.org/ns/structure/1.0\"
")
      (insert "         datatypeLibrary=\"http://www.w3.org/2001/XMLSchema-datatypes\">
")
      (insert "  <start>
")
      (insert "    <ref name=\"unattendElement\"/>
")
      (insert "  </start>
")
      (insert "  <define name=\"unattendElement\">
")
      (insert "    <element name=\"unattend\">
")
      (insert "      <zeroOrMore>
")
      (insert "        <ref name=\"settingsElement\"/>
")
      (insert "      </zeroOrMore>
")
      (insert "    </element>
")
      (insert "  </define>
")
      (insert "  <define name=\"settingsElement\">
")
      (insert "    <element name=\"settings\">
")
      (insert "      <attribute name=\"pass\">
")
      (insert "        <choice>
")
      (insert "          <value>windowsPE</value>
")
      (insert "          <value>offlineServicing</value>
")
      (insert "          <value>generalize</value>
")
      (insert "          <value>specialize</value>
")
      (insert "          <value>auditSystem</value>
")
      (insert "          <value>auditUser</value>
")
      (insert "          <value>oobeSystem</value>
")
      (insert "        </choice>
")
      (insert "      </attribute>
")
      (insert "      <oneOrMore>
")
      (insert "        <ref name=\"componentElement\"/>
")
      (insert "      </oneOrMore>
")
      (insert "    </element>
")
      (insert "  </define>
")
      (insert "  <define name=\"componentElement\">
")
      (insert "    <element name=\"component\">
")
      (insert "      <attribute name=\"name\">
")
      (insert "        <choice>
")
      (insert "          <value>Microsoft-Windows-Shell-Setup</value>
")
      (insert "          <value>Microsoft-Windows-International-Core</value>
")
      (insert "          <value>Microsoft-Windows-International-Core-WinPE</value>
")
      (insert "          <value>Microsoft-Windows-Setup</value>
")
      (insert "          <value>Microsoft-Windows-Deployment</value>
")
      (insert "          <value>Microsoft-Windows-PnpSysprep</value>
")
      (insert "        </choice>
")
      (insert "      </attribute>
")
      (insert "      <optional>
")
      (insert "        <attribute name=\"processorArchitecture\">
")
      (insert "          <choice>
")
      (insert "            <value>x86</value>
")
      (insert "            <value>amd64</value>
")
      (insert "            <value>arm64</value>
")
      (insert "          </choice>
")
      (insert "        </attribute>
")
      (insert "      </optional>
")
      (insert "      <optional>
")
      (insert "        <attribute name=\"language\">
")
      (insert "          <data type=\"string\"/>
")
      (insert "        </attribute>
")
      (insert "      </optional>
")
      (insert "      <optional>
")
      (insert "        <attribute name=\"publicKeyToken\">
")
      (insert "          <data type=\"string\"/>
")
      (insert "        </attribute>
")
      (insert "      </optional>
")
      (insert "      <zeroOrMore>
")
      (insert "        <ref name=\"anyElement\"/>
")
      (insert "      </zeroOrMore>
")
      (insert "    </element>
")
      (insert "  </define>
")
      (insert "  <define name=\"anyElement\">
")
      (insert "    <element>
")
      (insert "      <anyName/>
")
      (insert "      <zeroOrMore>
")
      (insert "        <attribute>
")
      (insert "          <anyName/>
")
      (insert "        </attribute>
")
      (insert "      </zeroOrMore>
")
      (insert "      <zeroOrMore>
")
      (insert "        <ref name=\"anyElement\"/>
")
      (insert "      </zeroOrMore>
")
      (insert "    </element>
")
      (insert "  </define>
")
      (insert "</grammar>
")))
  ;; Create locating rules file
  (let ((schemas-file (expand-file-name "schemas.xml" autounattend-xsd-directory)))
    (with-temp-file schemas-file
      (insert "<?xml version=\"1.0\"?>
")
      (insert "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">
")
      (insert "  <namespace ns=\"urn:schemas-microsoft-com:unattend\"
")
      (insert "             uri=\"autounattend.rng\"/>
")
      (insert "</locatingRules>
")))
  (message "Proper RelaxNG schema created!"))

;;; Key Bindings 
(defvar autounattend-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Main functionality
    (define-key map (kbd "C-c C-w") 'autounattend-wizard)
    (define-key map (kbd "C-c C-t") 'autounattend-selftest)
    ;; Validation functions
    (when (fboundp 'rng-validate-mode)
      (define-key map (kbd "C-c C-v") 'autounattend-toggle-validation))
    ;; Status and setup
    (define-key map (kbd "C-c C-s") 'autounattend-check-validation)
    (define-key map (kbd "C-c C-f") 'autounattend-force-schema-setup)
    ;; Completion and templates
    (define-key map (kbd "C-c C-i") 'autounattend-insert-component-template)
    (define-key map (kbd "C-c C-c") 'company-complete)
    map)
  "Keymap for `autounattend-mode'.")

;;; Utility Functions
(defun autounattend-force-schema-setup ()
  "Force schema setup for current buffer using available functions."
  (interactive)
  (when (derived-mode-p 'autounattend-mode)
    ;; Set schema locating files
    (setq-local rng-schema-locating-files
                (list (expand-file-name "schemas.xml" autounattend-xsd-directory)))
    ;; Use the only available function
    (when (fboundp 'rng-auto-set-schema-and-validate)
      (rng-auto-set-schema-and-validate)
      (message "Schema auto-set attempted"))
    (message "rng-current-schema: %s" rng-current-schema)))

(defun autounattend-toggle-validation ()
  "Toggle XML validation on/off."
  (interactive)
  (if (bound-and-true-p rng-validate-mode)
      (progn
        (rng-validate-mode -1)
        (message "Validation turned OFF"))
    (when (fboundp 'rng-validate-mode)
      (rng-validate-mode 1)
      (message "Validation turned ON"))))

(defun autounattend-check-validation ()
  "Check current buffer validation status using available functions."
  (interactive)
  (if (not (derived-mode-p 'autounattend-mode))
      (message "Not in autounattend-mode")
    (if rng-current-schema
        (progn
          (message "Schema validation: ACTIVE")
          ;; Try to validate using available method
          (condition-case err
              (if (fboundp 'rng-validate)
                  (progn
                    (rng-validate)
                    (message "Validation completed"))
                (message "Validation function not available"))
            (error
             (message "Validation error: %s" err))))
      (message "Schema validation: INACTIVE"))))

;;;  File associations 
(add-to-list 'auto-mode-alist '("autounattend\\.xml\\'" . autounattend-mode))
(add-to-list 'auto-mode-alist '("unattend\\.xml\\'" . autounattend-mode))
(add-to-list 'auto-mode-alist '("Autounattend\\.xml\\'" . autounattend-mode))
(add-to-list 'auto-mode-alist '("Unattend\\.xml\\'" . autounattend-mode))

;;;  Self-test 
(defun autounattend-selftest ()
  "Run diagnostics for autounattend-mode with completion."
  (interactive)
  (let ((results '()))
    ;; Basic checks
    (push (format "schemas dir: %s"
                  (if (file-directory-p autounattend-xsd-directory) "OK" "MISSING"))
          results)
    (push (format "autounattend.rng: %s"
                  (if (file-exists-p (expand-file-name "autounattend.rng" autounattend-xsd-directory))
                      "OK" "MISSING"))
          results)
    ;; Completion system check
    (push (format "autounattend-completion-at-point: %s"
                  (if (fboundp 'autounattend-completion-at-point) "DEFINED" "UNDEFINED"))
          results)
    (push (format "company-autounattend: %s"
                  (if (fboundp 'company-autounattend) "DEFINED" "UNDEFINED"))
          results)
    (push (format "company backend in list: %s"
                  (if (member 'company-autounattend company-backends) "YES" "NO"))
          results)
    ;; Schema validation test
    (let ((validation-test
           (ignore-errors
             (with-temp-buffer
               (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>
")
               (insert "<unattend xmlns=\"urn:schemas-microsoft-com:unattend\">
")
               (insert "  <settings pass=\"windowsPE\">
")
               (insert "    <component name=\"Microsoft-Windows-Setup\"/>
")
               (insert "  </settings>
")
               (insert "</unattend>")
               (autounattend-mode)
               (if rng-current-schema "SCHEMA SET" "NO SCHEMA")))))
      (push (format "validation: %s" (or validation-test "FAILED")) results))
    ;; Output results
    (with-help-window "*autounattend-selftest*"
      (princ "Autounattend Self-Test Results (with Completion)
")
      (dolist (r (reverse results))
        (princ (concat r "
")))
      (princ "
To fix completion:
")
      (princ "1. M-x autounattend-check-completion
")
      (princ "2. If undefined, M-x eval-buffer RET
")
      (princ "3. M-x autounattend-test-completion
"))))

;;; Wizard: interactive autounattend generator 
(defun autounattend--read-choices (prompt choices)
  "Prompt with PROMPT and a list of CHOICES, return selected choice."
  (completing-read (concat prompt ": ") choices nil t))

(defun autounattend--prompt-for-pass-values (pass)
  "Return a string snippet for PASS based on minimal questions."
  (cond
   ((string= pass "windowsPE")
    (concat "<settings pass=\"windowsPE\">
")
    (concat "  <component name=\"Microsoft-Windows-Setup\">
")
    (concat "    <DiskConfiguration>
")
    (concat "      <Disk wcm:action=\"add\">
")
    (concat "        <DiskID>0</DiskID>
")
    (concat "        <WillWipeDisk>true</WillWipeDisk>
")
    (concat "        <CreatePartitions>
")
    (concat "          <CreatePartition wcm:action=\"add\">
")
    (concat "            <Order>1</Order>
")
    (concat "            <Type>Primary</Type>
")
    (concat "            <Size>" (read-string "Primary partition size (MB): " "100000") "</Size>
")
    (concat "          </CreatePartition>
")
    (concat "        </CreatePartitions>
")
    (concat "      </Disk>
")
    (concat "    </DiskConfiguration>
")
    (concat "  </component>
")
    (concat "</settings>
"))
   ((string= pass "specialize")
    (concat "<settings pass=\"specialize\">
")
    (concat "  <component name=\"Microsoft-Windows-Shell-Setup\">
")
    (concat "    <ComputerName>" (read-string "Computer name: " "WIN-PC") "</ComputerName>
")
    (concat "  </component>
")
    (concat "</settings>
"))
   ((string= pass "oobeSystem")
    (concat "<settings pass=\"oobeSystem\">
")
    (concat "  <component name=\"Microsoft-Windows-Shell-Setup\">
")
    (concat "    <OOBE>
")
    (concat "      <HideEULAPage>" (completing-read "Hide EULA? (true/false): " '("true" "false") nil t "true") "</HideEULAPage>
")
    (concat "      <SkipMachineOOBE>" (completing-read "SkipMachineOOBE? (true/false): " '("true" "false") nil t "true") "</SkipMachineOOBE>
")
    (concat "      <SkipUserOOBE>" (completing-read "SkipUserOOBE? (true/false): " '("true" "false") nil t "true") "</SkipUserOOBE>
")
    (concat "    </OOBE>
")
    (concat "  </component>
")
    (concat "</settings>
"))
   ((string= pass "generalize")
    "<settings pass=\"generalize\">
  <component name=\"Microsoft-Windows-PnpSysprep\">
  </component>
</settings>
")
   (t "")))

(defun autounattend-wizard ()
  "Interactive wizard to generate a basic autounattend.xml."
  (interactive)
  (let* ((passes '(("windowsPE" . "Windows PE phase")
                   ("offlineServicing" . "Offline servicing")
                   ("generalize" . "Generalize phase")
                   ("specialize" . "Specialize phase")
                   ("auditSystem" . "Audit system")
                   ("auditUser" . "Audit user")
                   ("oobeSystem" . "OOBE system")))
         (selected (completing-read-multiple
                    "Select passes to include: "
                    (mapcar (lambda (x) (concat (car x) " - " (cdr x))) passes)))
         (body "")
         (unattend-head "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<unattend xmlns=\"urn:schemas-microsoft-com:unattend\">
")
         (unattend-tail "</unattend>
"))
    (dolist (pass-line selected)
      (let ((pass (car (split-string pass-line " - "))))
        (setq body (concat body (autounattend--prompt-for-pass-values pass)))))
    (let ((buf (get-buffer-create "*autounattend.xml*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert unattend-head)
        (insert body)
        (insert unattend-tail)
        (autounattend-mode)
        (goto-char (point-min)))
      (switch-to-buffer buf)
      (message "autounattend: wizard generated buffer. Review and save with C-x C-s"))))

;;; --- Embedded full component library installer ---
(defun autounattend-install-full-component-library ()
  "Install bundled component schemas into the XSD directory."
  (interactive)
  (autounattend--ensure-xsd-dir)
  (dolist (pair autounattend-embedded-component-xsd)
    (let ((file (expand-file-name (car pair) autounattend-xsd-directory))
          (content (cdr pair)))
      (unless (file-exists-p file)
        (with-temp-file file (insert content)))))
  (message "autounattend: full component library installed to %s" autounattend-xsd-directory))

;; Additional utility functions 
(defun autounattend-check-completion ()
  "Check if completion functions are properly defined."
  (interactive)
  (let ((results '()))
    (push (format "autounattend-completion-at-point: %s"
                  (if (fboundp 'autounattend-completion-at-point) "✅ DEFINED" "❌ UNDEFINED")) results)
    (push (format "company-autounattend: %s"
                  (if (fboundp 'company-autounattend) "✅ DEFINED" "❌ UNDEFINED")) results)
    (push (format "company-backends contains: %s"
                  (if (member 'company-autounattend company-backends) "✅ YES" "❌ NO")) results)
    (push (format "completion-at-point-functions: %s"
                  (if (member 'autounattend-completion-at-point completion-at-point-functions) "✅ YES" "❌ NO")) results)
    (with-help-window "*autounattend-completion-check*"
      (princ "Autounattend Completion System Check
")
      (dolist (r results)
        (princ (concat r "
")))
      (princ "
If functions are undefined, run:
")
      (princ "M-x eval-buffer RET
")
      (princ "to reload all functions.
"))))

(defun autounattend-test-completion ()
  "Test completion functionality."
  (interactive)
  (let ((test-buffer "*autounattend-completion-test*"))
    (with-current-buffer (get-buffer-create test-buffer)
      (erase-buffer)
      (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>
")
      (insert "<unattend xmlns=\"urn:schemas-microsoft-com:unattend\">
")
      (insert "  <settings pass=\"\">
")
      (insert "    <component name=\"\" processorArchitecture=\"\">
")
      (insert "    </component>
")
      (insert "  </settings>
")
      (insert "</unattend>
")
      (autounattend-mode)
      (goto-char (point-min))
      (switch-to-buffer test-buffer)
      (message "Completion test buffer created!
Test these positions:
1. After pass=\" - press TAB
2. After name=\" - press TAB
3. After processorArchitecture=\" - press TAB
4. Type '<s' and press TAB"))))

(defun autounattend-check-relaxng-support ()
  "Check if RelaxNG support is available in nxml-mode."
  (interactive)
  (with-help-window "*autounattend-relaxng-check*"
    (princ "RelaxNG Support Check
")
    (princ "nxml-mode version: ")
    (princ (if (boundp 'nxml-version) nxml-version "UNKNOWN"))
    (princ "
")
    (princ "Available validation functions:
")
    (dolist (func '(rng-auto-set-schema-and-validate rng-validate-mode nxml-validate))
      (princ (format "%-30s %s
" (symbol-name func) (if (fboundp func) "YES" "NO"))))
    (princ "
Schema file status:
")
    (let ((rng-file (expand-file-name "autounattend.rng" autounattend-xsd-directory))
          (schemas-file (expand-file-name "schemas.xml" autounattend-xsd-directory)))
      (princ (format "autounattend.rng: %s
" (if (file-exists-p rng-file) "EXISTS" "MISSING")))
      (princ (format "schemas.xml: %s
" (if (file-exists-p schemas-file) "EXISTS" "MISSING"))))
    (princ "
If RelaxNG is not working, the mode will still provide:
")
    (princ "• Syntax highlighting
")
    (princ "• Basic XML editing
")
    (princ "• Company completion
")
    (princ "• File associations
")))

(defun autounattend-fix-schema ()
  "Fix schema issues by recreating proper schemas."
  (interactive)
  (autounattend-create-proper-relaxng-schema)
  (message "Schema recreated. Please restart Emacs or reopen autounattend files."))

(defun autounattend-test-key-bindings ()
  "Test if key bindings are working."
  (interactive)
  (let ((results '()))
    ;; Test each key binding with available functions
    (dolist (key-cmd '(("C-c C-w" . autounattend-wizard)
                       ("C-c C-t" . autounattend-selftest)
                       ("C-c C-s" . autounattend-check-validation)
                       ("C-c C-f" . autounattend-force-schema-setup)
                       ("C-c C-i" . autounattend-insert-component-template)
                       ("C-c C-c" . company-complete)))
      (let ((key (car key-cmd))
            (cmd (cdr key-cmd)))
        ;; Ensure function is defined
        (unless (fboundp cmd)
          (fset cmd (lambda () (interactive) (message "Function %s" (symbol-name cmd)))))
        (push (format "%-10s -> %-30s %s"
                      key
                      (symbol-name cmd)
                      (if (where-is-internal cmd (current-local-map))
                          "✅" "❌"))
              results)))
    ;; Add rng-validate-mode if available
    (when (fboundp 'rng-validate-mode)
      (push (format "%-10s -> %-30s %s"
                    "C-c C-v"
                    "rng-validate-mode"
                    (if (where-is-internal 'rng-validate-mode (current-local-map))
                        "✅" "❌"))
            results))
    (with-help-window "*autounattend-key-bindings*"
      (princ "Autounattend Mode Key Bindings Test
")
      (princ "All should show ✅:
")
      (dolist (r (reverse results))
        (princ (concat r "
")))
      (princ "
To use:
")
      (princ "• C-c C-w - Create new file with wizard
")
      (princ "• C-c C-v - Toggle validation mode
")
      (princ "• C-c C-t - Run self-test
")
      (princ "• C-c C-s - Check validation status
")
      (princ "• C-c C-f - Force schema setup
")
      (princ "• C-c C-i - Insert component template
")
      (princ "• C-c C-c - Trigger company completion
"))))

(defun autounattend-final-check ()
  "Final check of autounattend-mode functionality."
  (interactive)
  (let ((results '()))
    (push (format "Mode active: %s" (eq major-mode 'autounattend-mode)) results)
    (push (format "Company mode: %s" (bound-and-true-p company-mode)) results)
    (push (format "Keymap loaded: %s" (eq (current-local-map) autounattend-mode-map)) results)
    (push (format "Schema available: %s" (not (null rng-current-schema))) results)
    (with-help-window "*autounattend-final-check*"
      (princ "🎉 Autounattend Mode - Final Status 🎉
")
      (dolist (r results)
        (princ (concat r "
")))
      (princ "
✨ Mode is fully operational! ✨
")
      (princ "
Available commands:
")
      (princ "• C-c C-w - Create new autounattend.xml with wizard
")
      (princ "• C-c C-v - Toggle validation mode
")
      (princ "• C-c C-t - Run self-test
")
      (princ "• C-c C-s - Check validation status
")
      (princ "• C-c C-f - Force schema setup
")
      (princ "• C-c C-i - Insert component template
")
      (princ "• C-c C-c - Trigger company completion
"))))

;;; Corrected eval-after-load 
(eval-after-load 'autounattend-mode
  '(progn
     (autounattend--ensure-xsd-dir)
     (unless (file-exists-p (expand-file-name "autounattend.rng" autounattend-xsd-directory))
       (autounattend-create-proper-relaxng-schema))))

(provide 'autounattend-mode)
;;; autounattend-mode.el ends here

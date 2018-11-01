
(require 'cl-lib)
(require 'doc-view)

(defgroup arm-lookup ()
  "Options for arm instruction set lookup."
  :group 'extensions)

(defcustom arm-lookup-txt nil
  "Path to ARM's manual processed through pdftotext."
  :group 'arm-lookup
  :type '(choice (const nil)
                 (file :must-match t)))

(defvar arm-lookup-pdf)

(setq arm-lookup-pdf (concat (file-name-sans-extension arm-lookup-txt) ".pdf"))

(defvar arm-lookup-index nil
  "Alist mapping instructions to page numbers.")

(defcustom arm-lookup-cache-directory
  (expand-file-name "arm-lookup" user-emacs-directory)
  "Directory where the PDF mnemonic index with be cached."
  :type 'string)


(defcustom arm-lookup-browse-pdf-function #'arm-lookup-browse-pdf-any
  "A function that launches a PDF viewer at a specific page.
This function accepts two arguments: filename and page number."
  :group 'arm-lookup
  :type '(choice (function-item :tag "First suitable PDF reader" :value
                                arm-lookup-browse-pdf-any)
                 (function-item :tag "Evince" :value
                                x86-lookup-browse-pdf-evince)
                 (function-item :tag "Xpdf" :value
                                x86-lookup-browse-pdf-xpdf)
                 (function-item :tag "Okular" :value
                                x86-lookup-browse-pdf-okular)
                 (function-item :tag "gv" :value
                                x86-lookup-browse-pdf-gv)
                 (function-item :tag "zathura" :value
                                x86-lookup-browse-pdf-zathura)
                 (function-item :tag "MuPDF" :value
                                x86-lookup-browse-pdf-mupdf)
                 (function-item :tag "Sumatra PDF" :value
                                x86-lookup-browse-pdf-sumatrapdf)
                 (function-item :tag "browse-url"
                                :value x86-lookup-browse-pdf-browser)
                 (function :tag "Your own function")))

(defun arm-lookup--index-file (txt)
  "Return index filename from txt filename."
  (concat (sha1 txt) "_v0"))

(defvar arm-lookup--expansions
  '(("^B.next$"
     "MI" "HI" "CS")
    )
  "How to expand mnemonics into multiple mnemonics.")


(defun arm-lookup--expand (names page)
  "Expand string of PDF-sourced mnemonics into user-friendly mnemonics."
  (let ((case-fold-search nil)
        (rev-string-match-p (lambda (s re) (string-match re s))))
    (save-match-data
      (cl-loop for mnemonic-raw in (split-string names " */ *")
               ;; Collapse "int 3" and "int n" into "int"
               for mnemonic = (replace-regexp-in-string " .+$" "" mnemonic-raw)
               for (_ . tails) = (cl-assoc mnemonic arm-lookup--expansions
                                           :test rev-string-match-p)
               nconc (cl-loop for tail in tails
                              for rep = (replace-match tail nil nil mnemonic 1)
                              collect (cons (downcase rep) page))))))


(cl-defun arm-lookup-create-index (&optional (txt arm-lookup-txt))
  "Create an index alist from PDF mapping mnemonics to page numbers.
   This function requires the pdftotext command line program."
  (let ((mnemonic "C[6,7].*\n.*\n.*\nC[6,7]\\.2\\.[0-9]+\n\n\\([A-Z]+\\(?:\\.[a-z]+\\)?\\)")
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (case-fold-search t))
    (with-temp-buffer
      (insert-file-contents txt)
      (setf (point) (point-min))
      (cl-loop for page upfrom 1
               while (< (point) (point-max))
               ;; look at first character at every page
               when (looking-at mnemonic)
               collect (cons (match-string 1) (+ 3 page)) into index
               do (forward-page)
               finally (cl-return
                        (cl-remove-duplicates
                         index :key #'car :test #'string= :from-end t)
               )))))


(defun arm-lookup--save-index (txt index)
  "Save INDEX for PDF in `arm-lookup-cache-directory'."
  (let* ((index-file (arm-lookup--index-file txt))
         (cache-path (expand-file-name index-file arm-lookup-cache-directory)))
    (mkdir arm-lookup-cache-directory t)
    (with-temp-file cache-path
      (prin1 index (current-buffer)))
    index))


(defun arm-lookup--load-index (txt)
  "Return index PDF from `arm-lookup-cache-directory'."
  (let* ((index-file (arm-lookup--index-file txt))
         (cache-path (expand-file-name index-file arm-lookup-cache-directory)))
    (when (file-exists-p cache-path)
      (with-temp-buffer
        (insert-file-contents cache-path)
        (setf (point) (point-min))
        (ignore-errors (read (current-buffer)))))))



(defun arm-lookup-ensure-index ()
  "Ensure the PDF index has been created, returning the index."
  (when (null arm-lookup-index)
    (cond
     ((null arm-lookup-txt)
      (error "No txt available. Set `arm-lookup-txt'."))
     ((not (file-exists-p arm-lookup-txt))
      (error "Txt not found. Check `arm-lookup-txt'."))
     ((setf arm-lookup-index (arm-lookup--load-index arm-lookup-txt))
      arm-lookup-index)
     ((progn
        (message "Generating mnemonic index ...")
        (setf arm-lookup-index (arm-lookup-create-index))
        (arm-lookup--save-index arm-lookup-txt arm-lookup-index)))))
  arm-lookup-index)

(defun arm-lookup-ensure-and-update-index ()
  "Ensure the PDF index has been created and (unconditionally) updated.
   Useful for forcibly syncing the index with the current PDF
   without resorting to manual deletion of index file on
   filesystem."
  (interactive)
  (cond
   ((null arm-lookup-txt)
    (error "No textfile available. Set `arm-lookup-txt'."))
   ((not (file-exists-p arm-lookup-txt))
    (error "Txt not found. Check `arm-lookup-txt'."))
   ((message "Generating mnemonic index ...")
    (setf arm-lookup-index (arm-lookup-create-index))
    (arm-lookup--save-index arm-lookup-txt arm-lookup-index)
    (message "Finished generating mnemonic index."))))

(defun arm-lookup-browse-pdf (pdf page)
  "Launch a PDF viewer using `arm-lookup-browse-pdf-function'."
  (funcall arm-lookup-browse-pdf-function pdf page))


;;;###autoload
(defun arm-lookup (mnemonic)
  "Jump to the PDF documentation for MNEMONIC.
Defaults to the mnemonic under point."
  (interactive
   (progn
     (arm-lookup-ensure-index)
     (let* ((mnemonics (mapcar #'car arm-lookup-index))
            (thing (thing-at-point 'word))
            (mnemonic (if (member thing mnemonics) thing nil))
            (prompt (if mnemonic
                        (format "Mnemonic (default %s): " mnemonic)
                      "Mnemonic: ")))
       (list
        (completing-read prompt mnemonics nil t nil nil mnemonic)))))
  (let ((page (cdr (assoc mnemonic arm-lookup-index))))
    (arm-lookup-browse-pdf (file-truename arm-lookup-pdf) page)))

;; PDF viewers:

(defun arm-lookup-browse-pdf-pdf-tools (pdf page)
  "View PDF at PAGE using Emacs' `pdf-view-mode' and `display-buffer'."
  (require 'pdf-tools)
  (prog1 t
    (with-selected-window (display-buffer (find-file-noselect pdf :nowarn))
      (with-no-warnings
        (pdf-view-goto-page page)))))

(defun arm-lookup-browse-pdf-doc-view (pdf page)
  "View PDF at PAGE using Emacs' `doc-view-mode' and `display-buffer'."
  (prog1 t
    (unless (doc-view-mode-p 'pdf)
      (error "doc-view not available for PDF"))
    (with-selected-window (display-buffer (find-file-noselect pdf :nowarn))
      (doc-view-goto-page page))))

(defun arm-lookup-browse-pdf-xpdf (pdf page)
  "View PDF at PAGE using xpdf."
  (start-process "xpdf" nil "xpdf" "--" pdf (format "%d" page)))

(defun arm-lookup-browse-pdf-evince (pdf page)
  "View PDF at PAGE using Evince."
  (start-process "evince" nil "evince" "-p" (format "%d" page) "--" pdf))

(defun arm-lookup-browse-pdf-okular (pdf page)
  "View PDF at PAGE file using Okular."
  (start-process "okular" nil "okular" "-p" (format "%d" page) "--" pdf))

(defun arm-lookup-browse-pdf-gv (pdf page)
  "View PDF at PAGE using gv."
  (start-process "gv" nil "gv" "-nocenter" (format "-page=%d" page) "--" pdf))

(defun arm-lookup-browse-pdf-zathura (pdf page)
  "View PDF at PAGE using zathura."
  (start-process "zathura" nil "zathura" "-P" (format "%d" page) "--" pdf))

(defun arm-lookup-browse-pdf-sumatrapdf (pdf page)
  "View PDF at PAGE using Sumatra PDF."
  (start-process "sumatrapdf" nil "sumatrapdf" "-page" (format "%d" page) pdf))

(defun arm-lookup-browse-pdf-mupdf (pdf page)
  "View PDF at PAGE using MuPDF."
  ;; MuPDF doesn't have a consistent name across platforms.
  ;; Furthermore, Debian ships with a broken "mupdf" wrapper shell
  ;; script and must be avoided. Here we use `executable-find' to
  ;; avoid calling it as mupdf-x11 on non-X11 platforms.
  (let ((exe (or (executable-find "mupdf-x11") "mupdf")))
    (start-process "mupdf" nil exe "--" pdf (format "%d" page))))

(defun arm-lookup-browse-pdf-browser (pdf page)
  "Visit PDF using `browse-url' with a fragment for the PAGE."
  (browse-url (format "file://%s#%d" pdf page)))



(defun arm-lookup-browse-pdf-any (pdf page)
  "Try visiting PDF using the first viewer found."
  (or (ignore-errors (arm-lookup-browse-pdf-pdf-tools pdf page))
      (ignore-errors (arm-lookup-browse-pdf-doc-view pdf page))
      (ignore-errors (arm-lookup-browse-pdf-evince pdf page))
      (ignore-errors (arm-lookup-browse-pdf-xpdf pdf page))
      (ignore-errors (arm-lookup-browse-pdf-okular pdf page))
      (ignore-errors (arm-lookup-browse-pdf-gv pdf page))
      (ignore-errors (arm-lookup-browse-pdf-zathura pdf page))
      (ignore-errors (arm-lookup-browse-pdf-mupdf pdf page))
      (ignore-errors (arm-lookup-browse-pdf-sumatrapdf pdf page))
      (ignore-errors (arm-lookup-browse-pdf-browser pdf page))
      (error "Could not find a PDF viewer.")))

(provide 'arm-lookup)

;;; jl-arm-lookup.el ends here

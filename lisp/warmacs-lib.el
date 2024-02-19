;; warmacs-lib.el -*- lexical-binding: t; -*-

;; Warmac's standard lib

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE. "
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(defmacro use-layer! (LAYERNAME &rest body)
  "Load a layer using use-package. LAYERNAME is the name of the layer."
  (declare (indent 2))
  (let ((layer-dir (expand-file-name (symbol-name LAYERNAME) warmacs-layers-dir))
	(init-feature (intern (format "%s/init" LAYERNAME))))
    `(progn
       ;; Unload if feature is loaded, so that new changes are loaded
       ;; This check is not required on startup
       ;; TODO optimising for only when loading after startup might save time
       (when (featurep (quote ,init-feature))
	 (unload-feature (quote ,init-feature) t))
       ;; Load the layer using use-package
       (use-package ,init-feature
	 :ensure nil
         :load-path ,layer-dir
         ,@body)

       ;; Add the layer to the list of loaded layers
       (add-to-list 'warmacs-layers ',LAYERNAME)
       (setq warmacs-layers (delete-dups warmacs-layers)))))

(provide 'warmacs-lib)

;; warmacs-lib.el -*- lexical-binding: t; -*-

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE. "
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(provide 'warmacs-lib)

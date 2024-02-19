;; -*- lexical-binding: t; -*-

;;
;; Scala Layer

(use-package scala-mode)

(use-package sbt-mode
  :after scala-mode
  :commands (sbt-start sbt-command))

(provide '+lang/scala/init)

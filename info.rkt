#lang setup/infotab
(define name "Slideshow LaTeX")
(define blurb
  (list "LaTeX in Slideshow"))
(define scribblings '(["main.scrbl" (multi-page)]))
(define categories '(misc))
(define primary-file "main.rkt")
(define compile-omit-paths '("example.rkt"))
(define release-notes 
  (list
   '(ul (li "Initial release"))))
(define repositories '("4.x"))

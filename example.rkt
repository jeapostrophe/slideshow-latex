#lang slideshow
(require racket/runtime-path
         "main.rkt"
         "extras.rkt"
         slideshow/code)

;; latex2bitmap caches renderings, identifying them by md5 hashes of the
;; contents of the tex files + the DPI they were rendered at

;; This requires the --trust flag when launched from the command line

;; Cache pngs in a subdirectory; without this, it's the user's temp dir
(setup-local-latex-cache)

;; We want to see the whole log when there are errors (the log parser doesn't
;; always find them, because LaTeX doesn't have a standard error format), and
;; we want little info messages about whether pngs are built or just loaded
(latex-debug? #t)

;; Added to every tex file, before the document proper:
(add-preamble #<<latex
\usepackage{amsmath, amssymb}
\newcommand{\targetlang}{\lambda_{\mathrm{ZFC}}}
\newcommand{\meaningof}[1]{[\![{#1}]\!]}  % use in semantic functions
\newcommand{\A}{\mathcal{A}}
\renewcommand{\P}{\mathbb{P}}
\newcommand{\N}{\mathbb{N}}
\newcommand{\Z}{\mathbb{Z}}
\newcommand{\Q}{\mathbb{Q}}
\newcommand{\R}{\mathbb{R}}
latex
              )

(define-runtime-path main-background-path
  "images/plt-background1-main-1024x768.png")
(background-image (bitmap main-background-path))
(slide
 (scale/improve-new-text (soft-dropshadow (bt "The Title Page")) 3)
 (hrule)
 (scale
  (hbl-append (t "Discrete ") ($"\\P") (t "robability"))
  2)
 (scale/improve-new-text
  (hbl-append (t "Discrete ") ($"\\P") (t "robability"))
  2)
 (hrule)
 (scale/improve-new-text
  (para #:align 'center "in" (outline ($"\\targetlang")))
  3))

(define-runtime-path slides-background-path
  "images/plt-background1-slides-1024x768.png")
(background-image (bitmap slides-background-path))
(slide-number 2)
(slide
 #:title "Text styles"
 (soft-dropshadow (para #:align 'center "Soft dropshadow (and in LaTeX too:" ($"d^2 = x^2 + y^2") ")"))
 (dropshadow (para #:align 'center "Sharp dropshadow"))
 (dropshadow (para #:align 'center
                   "Sharp" (code 2 2 #:color "blue" #:opacity 3/4) "dropshadow")
             2 2 #:color "blue" #:opacity 3/4)
 'alts
 (list (list (para #:align 'center "Outlined Text is Teh Win"))
       (list (outline (para #:align 'center "Outlined Text is Teh Win")
                      #:color "red")
             (para #:align 'center "and is the same size as normal text"))))


(add1-slide-number)
(slide
 #:title "Blocks, labels and references (1)"
 (theorem "discrete stimulation" "All nonnegative integers are interesting.")
 (proof "Suppose that some nonnegative integers are uninteresting. Because" ($"<") "well-orders" ($"\\Z^+") ", there exists a smallest uninteresting nonnegative integer. But that's pretty interesting.")
 (item "By" (ref "discrete stimulation") "we see that any attempt to prove"
       "that some nonnegative integers are mundane is doomed.")
 (definition "interesting integers" ($"\\N") "is the set of interesting nonnegative integers.")
 )


(background-image (cellophane (scale/improve-new-text ($$"\\int e^{-x^2}") 8)
                              1/8))
(add1-slide-number)
(slide
 #:title "Blocks, labels and references (2)"
 (item "This should be" ($"\\dfrac{1}{\\sqrt{2\\pi}}") ":")
 (equation "Euler" (colorize ($$"\\int_{-\\infty}^\\infty e^{-x^2}\\,dx") "DarkRed"))
 (item "By cunning application of" (ref "Euler") ", something non-obvious is true")
 (item "This should be true:")
 (equation "Pythag" (colorize ($$"A^2 + B^2 = C^2") "DarkGreen"))
 (item "If" (ref "Pythag") "is not true, we have a crisis of triangular proportions"))


;; Remove all the temp files and pngs that weren't used
;; Would probably be better as an exit handler, though there are times I've
;; wanted to save unused pngs
(tidy-latex-cache)

#lang racket/base
(require (for-syntax racket/base)
         racket/system
         racket/string
         racket/port
         racket/list
         racket/vector
         racket/class
         racket/gui/base
         file/md5)

(provide latex->bitmap latex->bitmap+ascent
         latex-preamble add-preamble latex-dpi latex-debug?
         latex-path dvipng-path
         (struct-out exn:latex)
         (struct-out exn:dvipng)
         set-latex-cache-path
         tidy-latex-cache
         setup-local-latex-cache)

(define used-files (make-hash))

(define cached-bitmaps (make-hash))

;; Document contents
(define latex-preamble (make-parameter ""))

(define (add-preamble . latex-strs)
  (latex-preamble (string-join (cons (latex-preamble) latex-strs) "\n")))

;; Formatting options
(define latex-dpi (make-parameter 150))

;; Spam the console with debugging info?
(define latex-debug? (make-parameter #t))

(define (find-executable name [defaultdir "/usr/bin"])
  (cond [(find-executable-path name) => (λ (p) p)]
        [(find-executable-path (string-append name ".exe")) => (λ (p) p)]
        [else  (build-path defaultdir name)]))

;; Default paths
(define latex-path (make-parameter (find-executable "latex" "/usr/bin")))
(define dvipng-path (make-parameter (find-executable "dvipng" "/usr/bin")))
(define cache-path (make-parameter (find-system-path 'temp-dir)))

;; Exceptions raised
(struct exn:latex exn:fail (message))
(struct exn:dvipng exn:fail ())

;; base+ext : path? string? -> path?
(define (base+ext file-base ext)
  (build-path (string-append (path->string file-base) ext)))

;; text-file->string : path? -> string?
(define (text-file->string file)
  (call-with-input-file file #:mode 'text port->string))

;; string->text-file : string? path? -> void?
(define (string->text-file str file)
  (with-output-to-file file #:mode 'text #:exists 'truncate
    (lambda () (printf "~a" str))))

;; text-file->string-list : path? -> (listof string?)
(define (text-file->string-list file)
  (call-with-input-file file #:mode 'text port->lines))

;; Example error:
;; ./latex125249806.tex:18: Undefined control sequence.

;; parse-latex-log : path? -> string?
;; Returns the first error message and context string found in the log
(define (parse-latex-log base-file)
  (define error-regexp
    (regexp (format ".*~a.*:[0-9]+:" (path->string base-file))))
  
  (let loop ([lines  (text-file->string-list (base+ext base-file ".log"))])
    (cond [(empty? lines)  "unknown"]
          [(regexp-match error-regexp (first lines))
           (let* ([info  (regexp-split #rx":" (first lines))]
                  [msg  (string-join (drop info 2) ":")]
                  [ctx  (string-join (take (rest lines) 2) "\n")])
             (format "~a ~a" msg ctx))]
          [else  (loop (rest lines))])))

;; compile-latex : path? -> void?
(define (compile-latex file-base)
  (define tex-file (base+ext file-base ".tex"))
  (define dvi-file (base+ext file-base ".dvi"))
  (define png-file (base+ext file-base ".png"))
  
  (define latex-args
    (list "-interaction=batchmode" "-file-line-error" "-halt-on-error"
          (path->string tex-file)))
  
  (define dvipng-args
    (list "--follow" "-bg" "Transparent" "--truecolor"
          "-D" (number->string (exact->inexact (latex-dpi))) "-T" "tight"
          "-o" (path->string png-file) (path->string dvi-file)))
  
  (when (latex-debug?)
    (printf "INFO: ~a ~a~n" (latex-path) (string-join latex-args " ")))
  
  (when (not (zero? (parameterize ([current-output-port (open-output-nowhere)])
                      (apply system*/exit-code (latex-path) latex-args))))
    (when (latex-debug?)
      (printf "ERROR: latex error, here's the log:~n")
      (printf "~a~n" (text-file->string (base+ext file-base ".log"))))
    (raise (exn:latex "latex failed" (current-continuation-marks)
                      (parse-latex-log file-base))))
  
  (when (latex-debug?)
    (printf "INFO: ~a ~a~n" (dvipng-path) (string-join dvipng-args " ")))
  
  (parameterize ([current-output-port (open-output-nowhere)])
    (apply system*/exit-code (dvipng-path) dvipng-args)
    (when (not (file-exists? png-file))
      (raise (exn:dvipng (format "dvipng failed to convert ~e" dvi-file)
                         (current-continuation-marks))))))

;; latex-doc->file-base : string? -> string?
(define (latex-doc->file-base doc-str)
  (build-path
   (format "latex2bitmap-~a"
           (bytes->string/latin-1
            (call-with-input-string (string-append (number->string (latex-dpi))
                                                   doc-str)
                                    md5)))))

;; ensure-png : string? -> void?
(define (ensure-png file-base doc-str)
  (define tex-file (base+ext file-base ".tex"))
  (define png-file (base+ext file-base ".png"))
  
  (unless (file-exists? png-file)
    (when (latex-debug?) (printf "INFO: generating ~a~n" png-file))
    (string->text-file doc-str tex-file)
    (compile-latex file-base))
  
  (hash-set! used-files png-file #t))

;; latex->latex-doc : string? -> string?
(define (latex->latex-doc latex-str)
  (string-append "\\documentclass[12pt]{article}\n"
                 "\\pagestyle{empty}\n"
                 (format "~a~n" (latex-preamble))
                 (format "\\begin{document}~n")
                 (format "~a~n" latex-str)
                 "\\end{document}"))

;; latex->bitmap : string? -> bitmap%
(define (latex->bitmap latex-str)
  (define doc-str (latex->latex-doc latex-str))
  (define file-base (latex-doc->file-base doc-str))
  (define png-file (base+ext file-base ".png"))
  
  (hash-ref!
   cached-bitmaps file-base
   (λ () (parameterize ([current-directory  (cache-path)])
           (ensure-png file-base doc-str)
           (when (latex-debug?) (printf "INFO: loading ~a~n" png-file))
           (make-object bitmap% png-file 'png/mask #f)))))


(define (get-column-alpha bm x)
  (let* ([height  (send bm get-height)]
         [b  (make-bytes (* 4 height))])
    (send bm get-argb-pixels x 0 1 height b #t)
    (build-vector height (λ (y) (bytes-ref b (* 4 y))))))

(define (find-first-largest v [idx 0] [lidx 0])
  (cond [(idx . >= . (vector-length v))  lidx]
        [((vector-ref v idx) . > . (vector-ref v lidx))
         (find-first-largest v (add1 idx) idx)]
        [else  (find-first-largest v (add1 idx) lidx)]))

(define (blank-column? v)
  (zero? (vector-count (compose not zero?) v)))

(define (find-first-column bm x0)
  (cond [(x0 . >= . (send bm get-width))  (send bm get-width)]
        [(blank-column? (get-column-alpha bm x0))
         (find-first-column bm (add1 x0))]
        [else  x0]))

(define (latex->bitmap+ascent latex-str)
  (define 1/2px-in (exact->inexact (/ 1 (latex-dpi) 2)))
  (define back-in (exact->inexact (/ 5 (latex-dpi))))
  (define baseline-dot (format "\\rule{~ain}{~ain}" 1/2px-in 1/2px-in))
  
  (define bm (latex->bitmap (format "\\makebox[0px][l]{\\hspace{-~ain}~a}~a"
                                    back-in baseline-dot latex-str)))
  
  (define ascent (+ 1 (find-first-largest (get-column-alpha bm 0))))
  (define x0 (find-first-column bm 1))
  
  (values bm ascent x0))


(define (set-latex-cache-path cpath)
  (when (file-exists? cpath)
    (error 'set-cache "~e exists as a regular file" cpath))
  (when (not (directory-exists? cpath))
    (make-directory cpath))
  (when (latex-debug?) (printf "INFO: latex compile path set to ~a~n" cpath))
  (cache-path cpath))

(define (tidy-latex-cache)
  (for ([file  (directory-list (cache-path))])
    (when (and (regexp-match #rx"^latex2bitmap-" (path->string file))
               (not (hash-has-key? used-files file)))
      (when (latex-debug?) (printf "INFO: deleting unused file ~a~n" file))
      (delete-file (build-path (cache-path) file)))))

(define (setup-local-cache stx)
  (let ([src  (syntax-source stx)])
    (if (path? src)
        (let-values ([(base-dir modname _) (split-path src)])
          (set-latex-cache-path (path->directory-path
                                 (build-path base-dir (format "~a-latex" modname)))))
        (printf "WARNING: not a path: ~a~n" src))))

(define-syntax (setup-local-latex-cache stx)
  (syntax-case stx ()
    [(local-name)  #'(setup-local-cache #'local-name)]))

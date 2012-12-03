#lang racket/base
(require slideshow
         racket/draw)

(provide dropshadow soft-dropshadow outline hrule
         with-font-size with-font-scale
         bigger smaller font-half font-double
         with-main-font with-style with-styles
         bold italic comic-style
         with-font-family decorative roman script swiss modern
         background-image slide-number add1-slide-number format-slide-number
         text->picts encircle enbox crossout
         next-number label ref ref+label
         make-numbered-block-style definition-style theorem-style
         definition example theorem corollary lemma qed para+qed proof
         eqnlabel equation
         blockquote
         para-table para-row para-cell)

;; =============================================================================
;; Dropshadows and outlines

;; Limitation: these do not work on text that has already been colorized, such
;; as LaTeX, bitmaps, and colorized code

(define (maybe-colorize pict color)
  (if color (colorize pict color) pict))

;; Sharp dropshadow
;; h-amt and v-amt are in *pixels*, not slideshow units
(define (dropshadow pct [h-amt 1] [v-amt 1] #:color [color #f] #:opacity [opacity 1/2])
  (match-define (list wf hf) (current-expected-text-scale))
  (refocus
   (scale
    (lt-superimpose
     (inset (cellophane (maybe-colorize (scale pct wf hf) color) opacity) h-amt v-amt)
     (scale pct wf hf))
    (/ wf) (/ hf))
   pct))

;; Soft dropshadow
(define (soft-dropshadow pct #:color [color #f] #:opacity [opacity 1/2])
  (match-define (list wf hf) (current-expected-text-scale))
  (refocus
   (scale
    (lt-superimpose
     (inset (cellophane (maybe-colorize (scale pct wf hf) color) opacity) 1 1)
     (inset (cellophane (maybe-colorize (scale pct wf hf) color) (/ opacity 2)) 2 2)
     (scale pct wf hf))
    (/ wf) (/ hf))
   pct))

;; Outlined text
(define (outline pct #:color [color "white"] #:opacity [opacity 1/2])
  (for/fold ([pct  pct]) ([dx  (in-list '(0 -1 0 1))]
                          [dy  (in-list '(-1 0 1 0))])
    (dropshadow pct dx dy #:color color #:opacity opacity)))

;; Default title has soft dropshadow
(current-titlet
 (lambda (str)
   (colorize (soft-dropshadow (text str (current-main-font) 40))
             (current-title-color))))

;; =============================================================================
;; Slides with pict backgrounds and custom/sane slide numbers

(set-page-numbers-visible! #f)

;; Background parameters
(define background-image (make-parameter #f))
(define (background-image-pict)
  (define bg (background-image))
  (inset (scale bg (/ 1024 (pict-width bg)) (/ 768 (pict-height bg)))
         (- margin)))

;; Slide number parameters
(define slide-number (make-parameter #f))
(define (add1-slide-number) (slide-number (add1 (slide-number))))
(define format-slide-number
  (make-parameter
   (λ (num)
     (outline (scale (rt (number->string (slide-number))) 3/5)
              #:color "white" #:opacity 1))))

;; Slide assembly

(define (add-slide bg-pct pct)
  (refocus (ct-superimpose bg-pct pct) bg-pct))

(define (add-slide-number pct)
  (refocus
   (rb-superimpose pct ((format-slide-number) (slide-number)))
   pct))

(current-slide-assembler
 (let ([orig  (current-slide-assembler)])
   (λ (title sep body)
     (let* ([pct  (if (background-image)
                      (background-image-pict)
                      (inset (blank 1024 768) (- margin)))]
            [pct  (add-slide pct (orig title sep body))]
            [pct  (if (slide-number) (add-slide-number pct) pct)])
       pct))))

;; =============================================================================
;; Counters, labels, references, equations, theorems, proofs, etc.

;; These *should* play nicely with continuations

(define counters (make-parameter (hash)))

(define labels (make-parameter (hash)))

(define (next-number ctr)
  (define num (add1 (hash-ref (counters) ctr 0)))
  (counters (hash-set (counters) ctr num))
  num)

(define (label lab desc)
  (when (hash-has-key? (labels) lab)
    (printf 'make-label "label overwritten: ~e" lab))
  (labels (hash-set (labels) lab desc)))

(define (ref lab)
  (hash-ref (labels) lab (λ () (format "(unknown label ~e)" lab))))

(define (ref+label lab)
  (format "~a (~a)" (ref lab) lab))

(define ((make-numbered-block-style style name ctr) lab . items)
  (define num (next-number ctr))
  (label lab (format "~a ~a" name num))
  (style lab items))

(define (definition-style lab items)
  (apply para (bold (t (ref lab))) (format "(~a):" lab) items))

(define (theorem-style lab items)
  (apply para (bold (t (ref lab))) (format "(~a):" lab) items))

(define definition (make-numbered-block-style definition-style "Definition" 'definition))
(define example (make-numbered-block-style definition-style "Example" 'example))
(define theorem (make-numbered-block-style theorem-style "Theorem" 'theorem))
(define corollary (make-numbered-block-style theorem-style "Corollary" 'theorem))
(define lemma (make-numbered-block-style theorem-style "Lemma" 'theorem))

(define qed (t "□"))

(define (para+qed . items)
  (define qed (t "□"))
  (define par (apply para (append items (list (ghost qed)))))
  (pin-over par (- (pict-width par) (pict-width qed))
            (- (pict-height par) (pict-height qed)) qed))

(define (proof . items)
  (apply para+qed (bold (t "Proof.")) items))

(define (eqnlabel lab)
  (define num (next-number "equation"))
  (define desc (format "Equation ~a" num))
  (label lab desc)
  (t (format "(~a)" num)))

;; I can't remember why this is syntax...
(define-syntax-rule (equation lab items ...)
  (let ()
    (define desc (eqnlabel lab))
    (define par (para #:align 'center items ...))
    (pin-over par (- (pict-width par) (pict-width desc))
              (* 1/2 (- (pict-height par) (pict-height desc))) desc)))

;; =============================================================================
;; Useful miscellaneous junk

(define-syntax-rule (blockquote items ...)
  (parameterize ([current-para-width  (- (current-para-width) (* 4 gap-size))])
    (para items ...)))

(define (hrule)
  (hline (- (current-para-width) (* 2 gap-size)) (current-font-size)))

;; =============================================================================
;; Useless miscellaneous junk

(define-syntax-rule (with-font-size size e ...)
  (parameterize ([current-font-size  size]) e ...))

(define-syntax-rule (with-font-scale f e ...)
  (with-font-size (round (* f (current-font-size))) e ...))

(define-syntax-rule (bigger e ...) (with-font-scale 4/3 e ...))
(define-syntax-rule (smaller e ...) (with-font-scale 3/4 e ...))
(define-syntax-rule (font-half e ...) (with-font-scale 1/2 e ...))
(define-syntax-rule (font-double e ...) (with-font-scale 2 e ...))


(define-syntax-rule (with-main-font style e ...)
  (parameterize ([current-main-font  style]) e ...))


(define-syntax-rule (with-style sym e ...)
  (with-main-font (cons 'sym (current-main-font)) e ...))

(define-syntax-rule (with-styles (sym ...) e ...)
  (with-main-font (append (list 'sym ...) (current-main-font)) e ...))

(define-syntax-rule (bold e ...) (with-style bold e ...))
(define-syntax-rule (italic e ...) (with-style italic e ...))
(define-syntax-rule (comic-style e ...) (with-styles (bold italic caps) e ...))


(define (replace-family style sym)
  (cond [(symbol? style)  sym]
        [(cons? style)  (cons (car style) (replace-family (cdr style) sym))]))

(define-syntax-rule (with-font-family sym e ...)
  (with-main-font (replace-family (current-main-font) sym) e ...))

(define-syntax-rule (decorative e ...) (with-font-family 'swiss e ...))
(define-syntax-rule (roman e ...) (with-font-family 'roman e ...))
(define-syntax-rule (script e ...) (with-font-family 'script e ...))
(define-syntax-rule (swiss e ...) (with-font-family 'swiss e ...))
(define-syntax-rule (modern e ...) (with-font-family 'modern e ...))


(define (text->picts str)
  (apply vl-append (map t (regexp-split #rx"\n" str))))


(define (colorize+linewidth p c lw)
  (let* ([p  (if c (colorize p c) p)]
         [p  (if lw (linewidth lw p) p)])
    p))


(define (encircle p #:color [c "red"] #:line-width [lw (/ (current-font-size) 8)])
  (refocus
   (pin-over p
             (* 1/2 (- (current-font-size)))
             (* 1/2 (- (current-font-size)))
             (colorize+linewidth
              (ellipse (+ (pict-width p) (current-font-size))
                       (+ (pict-height p) (current-font-size)))
              c lw))
   p))


(define (enbox p #:color [c "red"] #:line-width [lw (/ (current-font-size) 8)])
  (refocus (frame (inset p (* 1/3 (current-font-size))) #:color c #:line-width lw) p))


(define (crossout p #:color [c "red"] #:line-width [lw (/ (current-font-size) 8)])
  (let* ([p  (pin-over p 0 0
                       (colorize+linewidth
                        (pip-line (pict-width p) (pict-height p) 0)
                        c lw))]
         [p  (pin-over p 0 (pict-height p)
                       (colorize+linewidth
                        (pip-line (pict-width p) (- (pict-height p)) 0)
                        c lw))])
    p))

(define black-color (make-object color% 0 0 0))
#;; This doesn't look nice or shrink bitmaps properly
(define (smooth-bitmap filename)
  (let ([bm (cond [(bitmap-draft-mode) #f]
                  [(filename . is-a? . bitmap%) filename]
                  [(filename . is-a? . image-snip%) (send filename get-bitmap)]
                  [else (make-object bitmap% filename 'unknown/mask)])])
    (if (and bm (send bm ok?))
        (let ([w (send bm get-width)]
              [h (send bm get-height)])
          (dc
           (lambda (dc x y)
             (if (dc . is-a? . bitmap-dc%)
                 (send dc draw-bitmap-section-smooth
                       bm x y w h 0 0 w h)
                 (send dc draw-bitmap bm x y 'solid black-color (send bm get-loaded-mask))))
           w h))
        (frame (inset (colorize (text "bitmap failed") "red") 2)))))


;; valign ::= 'top | 'top/baseline | 'center | 'bottom | 'bottom/baseline
(define valign->append
  (let ([appends  (make-immutable-hash (list (cons 'top ht-append)
                                             (cons 'top/baseline htl-append)
                                             (cons 'center hc-append)
                                             (cons 'bottom hb-append)
                                             (cons 'bottom/baseline hbl-append)))])
    (λ (valign)
      (hash-ref appends valign
                (λ () (error 'valign->append
                             "unknown vertical alignment: ~e" valign))))))

(define (car+cdr/improper ws null-car)
  (cond [(null? ws)  (values (null-car) null)]
        [(cons? ws)  (values (car ws) (cdr ws))]
        [else  (values ws ws)]))

(define (para-table col-widths
                    #:align [align 'center]
                    #:col-seps [col-seps gap-size]
                    #:row-aligns [row-aligns 'top/baseline]
                    #:row-seps [row-seps gap-size]
                    . ks)
  (define-values (as ss rows)
    (for/fold ([row-aligns row-aligns] [row-seps row-seps] [rows empty])
      ([k  (in-list ks)])
      (define-values (a as) (car+cdr/improper row-aligns (λ () 'center)))
      (define-values (s ss) (car+cdr/improper row-seps (λ () 0)))
      (values as ss (list* (blank 0 s) (k a col-widths col-seps) rows))))
  (para #:width (apply + col-widths) #:align align
        (apply vl-append (reverse (rest rows)))))

(define ((para-row . ks) align col-widths col-seps)
  (define-values (ss cols)
    (for/fold ([col-seps col-seps] [res empty])
      ([k  (in-list ks)] [w  (in-list col-widths)])
      (define-values (s ss) (car+cdr/improper col-seps (λ () 0)))
      (values ss (list* (blank s 0) (k w) res))))
  (apply (valign->append align) (reverse (rest cols))))

(define ((para-cell #:decode? [decode? #t] . elements) width)
  (define pct (para #:width width #:decode? decode? elements))
  (inset/clip pct 0 0 (- width (pict-width pct)) 0))

#lang racket/base
(require slideshow
         racket/draw
         "latex/latex2bitmap.rkt"
         "extras.rkt")

(provide add-preamble
         latex-page-width latex-pict lt $$ $
         (all-from-out "latex/latex2bitmap.rkt"))


;; the width of LaTeX pages in slideshow coordinates
(define latex-page-width (make-parameter client-w))

(define (lines . latex-strs)
  (string-join latex-strs "\n"))

; magic number warning - chosen by experimentation to make the small letters
; not look too much smaller than Slideshow's, and the big letters not too much
; bigger
(define (current-font-dpi)
  (* 7 (current-font-size)))

(define (px n)
  (format "~ain" (exact->inexact (/ n (current-font-dpi)))))

(define (minipage width . latex-strs)
  (lines (format "\\begin{minipage}{~a}" width)
         (apply lines latex-strs)
         (format "\\end{minipage}")))

(define (dpi-scale)
  (first (current-expected-text-scale)))

(define (latex->pict latex-str latex-stx)
  (with-handlers ([exn:latex?  (λ (e) (raise-syntax-error
                                       'latex (exn:latex-message e)
                                       latex-stx))])
    (define dscale (dpi-scale))
    (parameterize ([latex-dpi  (* dscale (current-font-dpi))])
      (define bm (latex->bitmap (minipage (px (latex-page-width)) latex-str)))
      (define w (send bm get-width))
      (define h (send bm get-height))
      (dc (λ (dest-dc x y)
            (define color (send (send dest-dc get-pen) get-color))
            (define opacity (send dest-dc get-alpha))
            (define new-bm (silhouette-bitmap bm color opacity))
            (define pct (scale (bitmap new-bm) (/ 1 dscale)))
            (draw-pict pct dest-dc x y))
          (/ w dscale) (/ h dscale)))))

(define (latex->pict/baseline latex-str latex-stx)
  (with-handlers ([exn:latex?  (λ (e) (raise-syntax-error
                                       'latex (exn:latex-message e)
                                       latex-stx))])
    (define dscale (dpi-scale))
    (parameterize ([latex-dpi  (* dscale (current-font-dpi))])
      (define-values (bm a x0) (latex->bitmap+ascent latex-str))
      (define w (- (send bm get-width) x0))
      (define h (send bm get-height))
      (define d (- h a))
      (dc (λ (dest-dc x y)
            (define color (send (send dest-dc get-pen) get-color))
            (define opacity (send dest-dc get-alpha))
            (define new-bm (silhouette-bitmap bm color opacity))
            (define pct
              (scale (drop-below-ascent
                      (inset/clip (bitmap new-bm) (- x0) 0 0 0) d)
                     (/ 1 dscale)))
            (draw-pict pct dest-dc x y))
          (/ w dscale) (/ h dscale) (/ a dscale) (/ d dscale)))))


(define-syntax (latex-pict stx)
  (syntax-case stx ()
    [(_ latex-str ...)
     (with-syntax ([latex-stx  stx])
       (syntax/loc stx
         (latex->pict (apply lines (list latex-str ...)) #'latex-stx)))]))

(define-syntax ($$ stx)
  (syntax-case stx ()
    [(_ math-str ...)  (syntax/loc stx (latex-pict "$$" math-str ... "$$"))]))


(define-syntax (lt stx)
  (syntax-case stx ()
    [(_ latex-str ...)
     (with-syntax ([latex-stx  stx])
       (syntax/loc stx
         (latex->pict/baseline (string-join (list latex-str ...) " ") #'latex-stx)))]))

(define-syntax ($ stx)
  (syntax-case stx ()
    [(_ math-str ...)  (syntax/loc stx (lt "$" math-str ... "$"))]))

;; =============================================================================
;; Functions for making bitmap silhouettes

; The interface for getting ARGB values is UNBELIEVABLY RETARDED and not at all
; well explained in the documentation. After hours of playing with this crap, I
; finally made the following two conversion functions, and I think they're
; correct.

(define (bitmap->argb-pixels bm)
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define bs (make-bytes (* 4 w h)))
  (send bm get-argb-pixels 0 0 w h bs #f)
  
  (define mask (send bm get-loaded-mask))
  (when mask
    (send mask get-argb-pixels 0 0 w h bs #t))
  
  bs)

(define (argb-pixels->bitmap bs w h)
  (unless (= (* 4 w h) (bytes-length bs))
    (error 'set-bitmap-argb-pixels!
           "~vx~v bitmap needs ~v argb bytes; given ~v bytes"
           w h (* 4 w h) (bytes-length bs)))
  
  (define bm (make-object bitmap% w h))
  (define dc (make-object bitmap-dc% bm))
  (send dc set-argb-pixels 0 0 w h bs #f)
  
  (define mask (make-object bitmap% w h))
  (send dc set-bitmap mask)
  (send dc set-argb-pixels 0 0 w h bs #t)
  (send bm set-loaded-mask mask)
  
  (send dc set-bitmap #f)
  
  bm)

(define (color->rgb col)
  (cond [(string? col)
         (let ([c  (send the-color-database find-color col)])
           (list (send c red) (send c green) (send c blue)))]
        [(col . is-a? . color%)
         (list (send col red) (send col green) (send col blue))]))

; Computing silhouettes is kinda pricey, so we cache them weakly
(define sil-cache (make-weak-hash))

(define (silhouette-bitmap bm color opacity)
  (match-define (list r g b) (color->rgb color))
  (hash-ref!
   sil-cache (list (eq-hash-code bm) opacity r g b)
   (λ ()
     (define w (send bm get-width))
     (define h (send bm get-height))
     (define bs (bitmap->argb-pixels bm))
     (for ([i  (in-range 0 (bytes-length bs) 4)])
       (define a-val (bytes-ref bs i))
       (when (a-val . > . 0)
         (bytes-set! bs i (inexact->exact (round (* a-val opacity))))
         (bytes-set! bs (+ 1 i) r)
         (bytes-set! bs (+ 2 i) g)
         (bytes-set! bs (+ 3 i) b)))
     (argb-pixels->bitmap bs w h))))

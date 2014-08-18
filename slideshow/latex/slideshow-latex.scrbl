#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/extract
          planet/scribble
          (for-label slideshow/latex
                     slideshow/extras))

@title[#:tag "top"]{Slideshow LaTeX}
@author[(author+email "Jay McCarthy" "jay@racket-lang.org")]

@defmodule[slideshow/latex]

This package provides a bunch of macros for including LaTeX figures in
your slideshows, plus some other goodies.

@margin-note{This package was written by Neil Toronto, but then
modified for distribution by Jay.}

The best way, right now, to use this package is to look at (and run) the example that it includes by running:
@racketmod[slideshow
(require tests/slideshow/latex/example)]
from DrRacket and then left-click the module name and @onscreen{Open} it and read the source. (Obviously, you need LaTeX installed in a sensible place.)

Hopefully we will write good documentation shortly. Maybe you can help?

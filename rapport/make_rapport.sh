#!/bin/bash

pdflatex summary.tex
bibtex summary
pdflatex summary.tex
xpdf summary.pdf

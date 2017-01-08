#!/bin/bash
pdflatex -shell-escape report
pdflatex -shell-escape report
bibtex report
pdflatex -shell-escape report
pdflatex -shell-escape report

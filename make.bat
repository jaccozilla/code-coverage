@echo off

set toolname="multi-file-code-coverage"

if "%1"=="" GOTO clean
if "%1"=="all" GOTO clean
if "%1"=="docs" GOTO docs
if "%1"=="clean" GOTO clean
GOTO error

:clean
rmdir /S /Q %toolname%
echo ---------- Planet Remove ----------
raco planet remove jowalsh %toolname%.plt 9 9
if "%1"=="clean" goto done

:planet
echo ---------- Building Planet Package ----------
mkdir %toolname%
xcopy *.rkt %toolname%\*
xcopy *.scrbl %toolname%\*
xcopy report\figures\* %toolname%\report\figures\*
echo ---------- Planet Create ----------
raco planet create %toolname%\
echo ---------- Planet fileinject ----------
raco planet fileinject jowalsh %toolname%.plt 9 9


if "%1"=="all" GOTO docs
GOTO done
:docs
echo ---------- Building Documentation ----------
if "%2"=="" GOTO error
echo \renewcommand{\preDoc}{\setcounter{page}{%2}} > report\scribble\page-number.tex
raco scribble ++style report\scribble\page-number.tex --dest report\scribble --latex code-coverage.scrbl
cd report\scribble
pdflatex code-coverage.tex
cd ..\..

GOTO done
:error
echo "usage: build [all  <page number>] [docs <page number>] [clean]"
:done
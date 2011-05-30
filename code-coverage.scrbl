#lang scribble/doc

@(require scribble/manual)
@(require "info-helper.rkt")

@(define (covered-files-dialog-name) "Covered Files Dialog")
@(define (uncovered-lines-dialog-name) "Uncovered Lines Dialog")

@title{@tool-name Tool}

The @tool-name Tool allows coverage information gathered from a single program evaluation to be displayed on multiple source files in multiple DrRacket windows.

@section{Installing the Tool}

Multi-File Code Coverage is a Planet package, however it only adds a tool rather than providing functions. To install evaluate the following program:

@racketmod[racket
           (require (planet jowalsh/code-coverage))]

... and then restart DrRacket. The "@button-label" button should now be visible as seen below.

@centered{
  @image["report/figures/coverage-button.png"]
}


@section{Using the Tool}

First ensure that you have "Syntactic Test Suite Coverage" enabled in the "Language->Choose Language..." dialog. You may need to click the "Show Details" button to see the expanded options. Then run the program you wish to collect multi-file coverage information for. Finally, click the "@button-label" button. This will send the coverage information to other open files and apply code coverage highlighting to them. The @secref[@(covered-files-dialog-name)] will also appear containing the list of files covered by the program you just ran.

You may then select one, or more, of the covered files to open and switch focus to. Additionally, by clicking the "@open-with-label", instead of just "Open", each selected file will spawn the @secref[@(uncovered-lines-dialog-name)] with a list of lines containing unevaluated expressions.


@section[#:tag @(covered-files-dialog-name)]{@(covered-files-dialog-name)}

The @secref[@(covered-files-dialog-name)] appears after clicking the "@button-label" button. It displays a list of files covered by the currently in-focus program and allows the selection of a file to open and switch focus to.


@centered{
  @image["report/figures/covered-files-dialog.png"]
  @(linebreak)
  @elem{@(covered-files-dialog-name)}
}

@subsection{Line Coverage Percent}

The percent in parentheses, next to each file in the @secref[@(covered-files-dialog-name)], is the percent of covered lines in that file. So 100% indicates that every line in that file is covered. Blank lines and comments are also counted as covered lines.

@subsection{Invalid Coverage Information}

An asterisk may appear next to a file in the @secref[@(covered-files-dialog-name)]. This indicates that the file has been modified since multi-file coverage information was last collected, which may have invalidated its coverage info. To ensure that all coverage information is valid run the program you are collecting multi-file code coverage for again.

@section[#:tag @(uncovered-lines-dialog-name)]{@(uncovered-lines-dialog-name)}

The @(uncovered-lines-dialog-name) contains a list of line numbers, for an individual file, that are not fully covered.

@centered{
  @image["report/figures/uncovered-lines-dialog.png"]
  @(linebreak)
  @elem{@(uncovered-lines-dialog-name)}
}

@section{Common Questions}

@subsection{Saving Coverage Information}

Whenever the "@button-label" button is clicked the multi-file code coverage information is saved to a file. This file is named <file name>@coverage-suffix and saved in the "compiled" directory next to the source file.

@subsection{My source file doesn't show up in the covered files dialog}

Only un-compiled files will appear in the @secref[@(covered-files-dialog-name)]. To ensure that all your covered files appear delete any "compiled" directories next to your source files. Then run your program again and click the "@button-label" button.




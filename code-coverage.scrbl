#lang scribble/doc

@(require scribble/manual)

@title{Multi-File Code Coverage Tool}

The Multi-File Code Coverage tool allows the coverage information gathered from a single program evaluation to be displayed on multiple source files in multiple DrRacket windows.

@section{Installing the Tool}

Multi-File Code Coverage is a Planet package, however it only adds a tool rather than providing functions. To install evaluate the following program:

@racketmod[racket
           (require (planet jowalsh/code-coverage))]

... and then restart DrRacket.

@section{Using the Tool}

First ensure that you have "Syntactic Test Suite Coverage" enabled in the "Language->Choose Language..." dialog. Then run the program you wish to collect multi-file coverage information for. Finally, click the "Load Code Coverage" button. A new dialog will apear with the list of file covered by the program you just ran. 

You may then select one, or more, of the covered files to open and switch focus to. Additinaly, by clicking the "Open With Uncovered Lines Dialog", instead of just "Open", each selected file will spawn an additional dialog with a list of lines containing unevaluated expressions.

@subsection{Line Coverage Percent}

The percent in parentheses, next to each file, is the percent of covered lines in that file. So 100% indicates that every line in that file is covered. Blank lines and comments are also counted as covered lines.

@subsection{Valid Coverage Information}

An asterisk may appear next to files in the covered files dialog. This indicates that the file has been modified since multi-file coverage information was last collected, which may have invalidated its coverage info. To ensure that all coverage information is valid run the program you are collecting code coverage for again.

@subsection{Why doesn't my source files =show up in the covered files dialog?}

Only un-compiled files will appear in the covered files dialog. To ensure that all your covered files appear delete any "compiled" directories next to your source files. Then run your program again and load code coverage.


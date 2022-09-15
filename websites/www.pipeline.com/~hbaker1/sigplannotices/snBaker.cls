%% Document class for SIGPLAN Notices Columns
%% for use with latex2e or latex3
%%
%% Author:  Bowden Wise (wiseb@cs.rpi.edu)
%% Adapted: Ron K. Cytron (cytron@cs.wustl.edu)
%%%
%%% Adapted further:  Seth Bergmann (bergmann@rowan.edu)
%%%    	For Henry Baker's customized column banner:
%%%		Remove the author line, delete "editor"
%%
%% This document class is used like this:
%%
% \documentclass{snBaker}
% \begin{document}
% \title{Your Title}
% \author{Your name and address}
% \maketitle
% \section{INTRODUCTION}
% Etc., etc. . . .
% \end{document}
% 
%
%%
\NeedsTeXFormat{LaTeX2e}
%%
\ProvidesClass{snBaker}
              [1996/10/15
 ACM SIGPLAN Notices Toolbox Document Class]

%%
%% OPTIONS 
%%

\DeclareOption{a5paper}
    {\ClassError{proc}{Option `a5paper' not supported}{}}
\DeclareOption{b5paper}
    {\ClassError{proc}{Option `b5paper' not supported}{}}
\DeclareOption{titlepage}%
    {\ClassError{proc}{Option `titlepage' not supported}{}}

\PassOptionsToClass{twoside,10pt}{article}

%%
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}

%%
\ProcessOptions

%%
\LoadClass{article}

%%
%% PACKAGES
%%

% Fonts
\usepackage{times}
\usepackage{avant}
\usepackage{epsf}
\usepackage{moreepsf}
\usepackage{latexsym}
% Graphics
% \usepackage{epsfig}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PAGE LAYOUT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% TEXT LAYOUT
                                     \newlength{\PageWidth}
  \setlength{\PageWidth}{8.375in}
                                      \newlength{\PageHeight}
   \setlength{\PageHeight}{10.875in}
                                      \newlength{\TopLogoHeight}
   \setlength{\TopLogoHeight}{0.5in}
                                      \newlength{\FootHeight}
   \setlength{\FootHeight}{1.0em}
                                      \newlength{\TextWidth}
   \setlength{\TextWidth}{6.50in}
                                      \newlength{\TopMargin}
   \setlength{\TopMargin}{0.50in}
                                      \newlength{\BotMargin}
   \setlength{\BotMargin}{0.50in}
                                      \newlength{\OuterMargin}
   \setlength{\OuterMargin}{0.75in}
                                      \newlength{\InnerMargin}
%
   \setlength{\headsep}{0.375in}
   \setlength{\headheight}{\TopLogoHeight}
%
   \setlength{\footskip}{0.375in}
   \addtolength{\footskip}{\FootHeight}
%
   \setlength{\topmargin}{-1.0in}
   \addtolength{\topmargin}{\TopMargin}
%
   \setlength{\textheight}{\PageHeight}
   \addtolength{\textheight}{-\TopMargin}
   \addtolength{\textheight}{-\headheight}
   \addtolength{\textheight}{-\headsep}
   \addtolength{\textheight}{-\footskip}
   \addtolength{\textheight}{-\BotMargin}
%
   \setlength{\oddsidemargin}{-1.0in}
   \addtolength{\oddsidemargin}{8.5in} % page width
   \addtolength{\oddsidemargin}{-\OuterMargin}
   \addtolength{\oddsidemargin}{-\TextWidth}
%
   \setlength{\evensidemargin}{-1.0in}
   \addtolength{\evensidemargin}{\OuterMargin}
%% 
   \setlength{\textwidth}{\TextWidth}
   \if@twocolumn
      \setlength{\columnsep}{.1875in}
   \fi

%%
%%
%% NO MARGINAL NOTES
\setlength{\marginparwidth}{0pt}
\setlength{\marginparsep}{0pt}
\setlength{\marginparpush}{0pt}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FONTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logo font:
\newcommand{\logofont}
           {\fontsize{30}{40}
            \fontfamily{pag}
            \fontseries{b}
            \fontshape{sl}
            \selectfont}

%
% Banner line title font 
%
% Title; 24pt; Times; Bold
\newcommand{\bannertitlefont}
           {\fontsize{20}{24}
            \fontfamily{ptm}
            \fontseries{bx}
            \fontshape{n}
            \selectfont}

% Main body fonts
% Title:       24pt; Times; Bold
% Author:      14pt; Times; Bold; Italic;
% Headings:    14pt; Times; Bold
% Subheadings: 12pt; Times; Bold

%% Title; 24pt; Times; Bold
\newcommand{\@titlefont}
           {\fontsize{18}{20}
            \fontfamily{ptm}
            \fontseries{b}
            \fontshape{n}
            \selectfont}

% Author font; 14pt; Times-Italc; Bold
\newcommand{\@authorfont}
           {\fontsize{14}{20}
            \fontfamily{ptm}
            \fontseries{b}
            \fontshape{it}
            \selectfont}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PARAGRAPHS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% separation between paragraphs, NO CHANGE, example:
% \parskip 1.5ex plus 1pt
\parskip=4pt plus 2pt minus 2pt

% no indentation of paragraphs, LET author specify
% \parindent 0pt

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PAGE STYLE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newsavebox{\OddLogoBox}
\newsavebox{\EvenLogoBox}
%% No page numbers
\pagestyle{empty}
\def\@oddhead{\hfil\usebox{\OddLogoBox}}
\def\@evenhead{\usebox{\EvenLogoBox}\hfil}
\def\@oddfoot{\hfil\thepage\hfil}
\def\@evenfoot{\hfil\thepage\hfil}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ENVIRONMENTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Redefine twocolumn so it doesnt do a clearpage
%% taken from latex.ltx

\def \bannertwocolumn {%
%%  \clearpage  %% removed so that twocolumn does not start new page
  \global\columnwidth\textwidth
  \global\advance\columnwidth-\columnsep
  \global\divide\columnwidth\tw@
  \global\hsize\columnwidth
  \global\linewidth\columnwidth
  \global\@twocolumntrue
  \global\@firstcolumntrue
  \col@number \tw@
  \@ifnextchar [\@topnewpage\@floatplacement
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TITLE PAGE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand{\dobanner}{%
{\flushleft{\bannertitlefont {\bf \@ColumnTitle}}}\\
{\flushright\vskip -0.25in
{\small {\bf Author:} {\em \@ColumnEditorName, \@ColumnEditorAffil}; 
{\it \@ColumnEditorEmail}}\\
}
\flushleft\vskip -.15in \rule{\hsize}{.02in}
\begin{center}
 {\@titlefont\sf\@ColumnSubTitle\par}
      \lineskip .5em%
    \vskip 1em%
%      \begin{tabular}[t]{c}%
%        \@authorfont \@ColumnAuthorName
%       \@ColumnAuthorName
%      \end{tabular}\par%
    \vskip 1em%
\end{center}
\par
\vskip 0.0em
}

%
% Redfine \maketitle to incorporate the banner across top of first page
%

\renewcommand{\maketitle}{%
\if@twocolumn
  \bannertwocolumn[\dobanner]
\else
  \dobanner
\fi
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MISC 
\savebox{\OddLogoBox}{\logofont Fill in the column title!}
\savebox{\EvenLogoBox}{\logofont Fill in the column title!}
\newcommand{\ColumnName}[2]{%
   \savebox{\OddLogoBox}{%
     %\rule{4pt}{\TopLogoHeight}%
     \makebox[0pt][r]{\PICGET{\hsize}{\TopLogoHeight}{logo}}%
     \makebox[0pt][r]{\logofont #1}%
   }%
   \savebox{\EvenLogoBox}{%
     %\rule{4pt}{\TopLogoHeight}%
     \makebox[0pt][l]{\PICGET{\hsize}{\TopLogoHeight}{logo}}%
     \makebox[0pt][l]{\logofont #1}%
   }%
   \setcounter{page}{#2}%
}
%
\def\@ColumnEditorName{Editor name}
\def\@ColumnEditorAffil{Editor affiliation}
\def\@ColumnEditorEmail{Editor email}
\newcommand{\ColumnEditor}[3]{%
   \def\@ColumnEditorName{#1}%
   \def\@ColumnEditorAffil{#2}%
   \def\@ColumnEditorEmail{#3}%
}
%
\gdef\@ColumnTitle{Column title}
\gdef\@ColumnSubTitle{Column subtitle}
\newcommand{\ColumnTitle}[1]{%
  \gdef\@ColumnTitle{#1}%
}
\newcommand{\ColumnSubTitle}[1]{%
  \gdef\@ColumnSubTitle{#1}%
}
\gdef\@ColumnAuthorName{Column author name}
\gdef\@ColumnAuthorBio{Column author bio}
\newcommand{\ColumnAuthor}[2]{%
   \gdef\@ColumnAuthorName{#1}%
   \gdef\@ColumnAuthorBio{#2}%
}
\newcommand{\SpewBio}[0]{%
   \vskip 4pt plus 8pt%
   \hrule%
   \nobreak%
   \vskip 4pt plus 8pt%
   \nobreak%
   \vbox{\em \noindent\@ColumnAuthorBio}%
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\flushbottom

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{FR16}[2019/01/21 ver. 0.0]
%Author: Francesco Ronchi (francescoronchi94@gmail.com)
\LoadClass[12pt, a4paper]{article}
%RUN WITH XeLaTeX

%--Packages--
%\RequirePackage[italian]{babel}
\RequirePackage[utf8]{inputenc}
\RequirePackage[T1]{fontenc}
\RequirePackage{latexsym}
\RequirePackage{amsmath}
\RequirePackage{graphicx}
\RequirePackage{xcolor}
\RequirePackage{setspace}
\RequirePackage{geometry}
\RequirePackage{marginnote}
\RequirePackage{caption}
\RequirePackage{siunitx}
\RequirePackage{eurosym}
\RequirePackage{colortbl}
\RequirePackage{array}
\RequirePackage{sectsty}
\RequirePackage[nottoc]{tocbibind}
\RequirePackage{float}
\RequirePackage{wrapfig}
\RequirePackage{vmargin}
    \setpapersize{A4}
    \setmarginsrb{30mm}{20mm}{30mm}{30mm}%left top right bottom
                 {8mm}{6mm}{10mm}{10mm}%headheight headsep footheight footskip
\definecolor{Sapienza}{RGB}{130,36,51}
\definecolor{Rosso}{RGB}{232,61,61}
\definecolor{Viola}{RGB}{188,30,188}
\definecolor{Celeste}{RGB}{34,186,211}
\definecolor{Arancione}{RGB}{232,181,19}
\definecolor{Blu}{RGB}{8,144,213}
\definecolor{Azzurro}{RGB}{57,186,238}
%\definecolor{Sapienza}{RGB}{111,10,25}
\definecolor{gold}{RGB}{199,153,0}
\RequirePackage{hyperref}
\hypersetup{colorlinks=true,linkcolor=black,allbordercolors=white}
\RequirePackage{enumitem} 
\renewcommand\labelitemi{\textbullet} 
\RequirePackage{pdflscape}
\RequirePackage{pgfplots}
\usetikzlibrary{shapes}
\RequirePackage{pgfplots}
\usetikzlibrary{intersections,positioning,calc}
\RequirePackage{tkz-euclide,tkz-fct}
\usetikzlibrary{angles,quotes}
\RequirePackage{lmodern}
\RequirePackage{titlesec}
\RequirePackage{tikz}
\usetikzlibrary{shapes}
\usetikzlibrary{arrows,positioning}
\RequirePackage{amsmath,bm}

\RequirePackage{mathspec}
\RequirePackage{xltxtra,xunicode}

%--Font-- 
\setmathsfont(Digits,Latin,Greek)[
Path = fonts/,Extension = .otf,
Scale=1.04,
UprightFont=ArnoPro-Regular,
BoldFont=ArnoPro-Bold,
ItalicFont=ArnoPro-Italic,
BoldItalicFont=ArnoPro-BoldItalic
]{ArnoPro}
\setmathrm
[Path = fonts/,Extension = .otf,
Scale=1.04,
UprightFont=ArnoPro-Regular,
BoldFont=ArnoPro-Bold,
ItalicFont=ArnoPro-Italic,
BoldItalicFont=ArnoPro-BoldItalic
]{ArnoPro}
\defaultfontfeatures{Mapping=tex-text}
\aliasfontfeatureoption{Ligatures}{Historic}{Historical}
\setromanfont
[Path = fonts/,
Extension = .otf,
Scale=1.04,
Mapping=tex-text, 
Ligatures={Common}, 
SizeFeatures={%
	{Size={-10}, Font=*-RegularCaption},
	{Size={10-13.0}, Font=*},
	{Size={13.01-19.9}, Font=*-RegularSubhead, Ligatures=Rare, Contextuals={Swash,Alternate}},
	{Size={19.91-},Font=*-RegularDisplay, Ligatures=Rare, Contextuals={Swash,Alternate}}},
UprightFont=ArnoPro-Regular,
BoldFont=ArnoPro-Bold,
ItalicFont=ArnoPro-Italic,
BoldItalicFont=ArnoPro-BoldItalic
]{ArnoPro}

\onehalfspacing


%--Titolo--
\renewcommand{\maketitle}{
\thispagestyle{empty}
\begin{figure}
\vspace{-1cm}
\centering
\includegraphics[width=0.6\textwidth]{univlogo.png}\\[1cm]
\end{figure}

\begin{center}
\textcolor{Blu}{\sc\centering\onehalfspacing\fontsize{4mm}{6mm}\selectfont Department od Economics, Management and Quantitative Methods\\Università degli Studi di Milano\\Data science and economics \\[1cm]}
\end{center}
\begin{center}
\Large\bfseries\sc Statistical Learning Project\\[0.5cm] \bfseries\Huge\sc Analysis and prediction models\\for New York City Airbnb \\[0.8cm]\normalsize\sc Supervised Learning Report\\ 
\end{center}

\vspace{5.5cm}
\begin{minipage}{0.4\textwidth}
\begin{flushleft} \large
\emph{Author:}\\
Andrea \textsc{Ierardi\\} % Your name
\end{flushleft}
\end{minipage}
~

\begin{center}
\vfill
{\sc Academic year 2019-2020}
\end{center}
}



%--Intestazione section--
\titleformat{\section}[display]
  {\normalfont\bfseries\color{Blu}}
  {\filleft
    \begin{tikzpicture}
    \node[
      outer sep=0pt,
      text width=2.5cm,
      minimum height=3cm,
      fill=white,
      font=\color{Blu}\fontsize{80}{0}\selectfont,
      align=center
      ] (num) {\thesection};
    \node[
      rotate=90,
      anchor=south,
      font=\color{black}\large\normalfont
      ] at ([xshift=-5pt]num.west) {\Huge\textsc{}};  
    \end{tikzpicture}%
  }
  {10pt}
  {\titlerule\vskip4pt\Huge\sc\sffamily}

%--Intestazione subsection--
\newcommand*\numb[1]{%
\begin{tikzpicture}[baseline=-0.7ex]
\node[
  outer sep=0pt,
      text width=0.6cm,
      minimum height=0.6cm,
      fill=Blu,
      font=\color{white}\fontsize{12}{20}\selectfont,
      align=center
      ] (num) {\thesubsection};
\end{tikzpicture}%
}
\titleformat{\subsection}
  {\normalfont\color{Blu}\large\sc\sffamily}{\numb{\thesubsection}}{0.8em}{}

%--Intestazione subsubsection--
\newcommand*\stocaz[1]{%
\begin{tikzpicture}[baseline=-0.7ex]
\node[
  outer sep=0pt,
      text width=0.8cm,
      minimum height=0.5cm,
      fill=Blu,
      font=\color{white}\fontsize{10}{20}\selectfont,
      align=center
      ] (num) {\thesubsubsection};
\end{tikzpicture}%
}
\titleformat{\subsubsection}
  {\normalfont\color{Blu}\bfseries\sc\sffamily}{\stocaz{\thesubsubsection}}{0.8em}{}
  
  

  
  
  
  
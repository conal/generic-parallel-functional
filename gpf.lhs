% -*- latex -*-

\documentclass[preprint
%if long
,nocopyrightspace
%endif
]{sigplanconf-old}

% I don't like links while drafting, since they result in a bookmarks pane
% in Acrobat Reader.  Turn off "draft" later.
\usepackage[colorlinks,urlcolor=black,citecolor=black,linkcolor=black]{hyperref} % ,draft=true

% \pagenumbering{arabic}

% author-date form
\usepackage[]{natbib}
\bibpunct();A{},
\let\cite=\citep

\input{macros}

%include polycode.fmt
%include forall.fmt
%include greek.fmt
%include mine.fmt

\title{Two generic functional parallel algorithms}

\authorinfo{Conal Elliott}{Target}{conal@@conal.net}

\bibliographystyle{plainnat}

\begin{document}

\maketitle

\end{document}

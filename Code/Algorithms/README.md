## Code/Algorithms

Mappen `Code/Algorithms` er til `.tex`-filer med beskrivelser af **Algorithms**oritmer, som laves med `algpseudocode`-pakken.

For eksempel, hvis man har en fil kaldet `Code/Algorithms/bubblesort.tex` med indholdet:

```latex
\begin{algorithm}[htbp]
  \begin{algorithmic}
    \Procedure{bubblesort}{\,\(a_{1}, \dotsc, a_{n}\) : reelle tal med \(n \geq 2\)\,}
    \For{\(i \coloneqq 1\) to \(n - 1\)}
    \For{\(j \coloneqq 1\) to \(n - i\)}
    \If{\(a_{j} > a_{j+1}\)}
    ombyt \(a_{j}\) og \(a_{j+1}\)
    \EndIf
    \EndFor
    \EndFor
    \EndProcedure
    \Comment{\(a_{1}, \dotsc, a_{n}\) er nu i voksende orden}
  \end{algorithmic}
  \caption{Bubblesort-algoritmen}
  \label{Algorithms:bubblesort}
\end{algorithm}
```

så kan algoritmen indsættes i dokumentet med kommandoen:

```latex
\input{Code/Algorithms/bubblesort}
```

% hscalc - Haskell Stack Calculator
% Copyright (C) 2007  Luis Cabellos
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

\begin{code}
module StackCalc( StackValue,
                  insertaDigito,
                  nullValue,
                  pilaVacia,
                  convertValues,
                  aplicaFuncion,
                  stringToVal
                ) where
\end{code}

\begin{code}
data StackValue = N Double |
                  T String
\end{code}

\begin{code}
instance Show StackValue where
    show (N v) = show v
    show (T v) = v
\end{code}

\begin{code}
stringToVal s = T s
\end{code}

\begin{code}
nullValue  = T "0"
\end{code}

\begin{code}
pilaVacia = [nullValue]
\end{code}

\begin{code}
extractDouble (N v) = v
extractDouble (T v) = read v
\end{code}

\begin{code}
convertValues xs = map toDouble xs
    where toDouble (N v) = N v
          toDouble (T v) = N (read v)
\end{code}

\begin{code}
insertaDigito ((T "0"):xs) n = T (show n) : xs
insertaDigito ((T s):xs) n = T (s ++ show n) : xs
insertaDigito ((N 0.0):xs) n = T (show n) : xs
insertaDigito (x:xs) n = T (show n) : x : xs
\end{code}

\begin{code}
aplicaFuncion [] _ = []
aplicaFuncion xs@(x:[]) _ = xs
aplicaFuncion (x:y:xs) f = N (f vy vx) : xs
    where vx = extractDouble x
          vy = extractDouble y
\end{code}

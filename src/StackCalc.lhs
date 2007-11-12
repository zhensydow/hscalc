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
module StackCalc( 
                 insertaDigito, 
                 pilaVacia,
                 aplicaFuncion
                ) where
\end{code}

\begin{code}
pilaVacia :: (Fractional n) => [n]
pilaVacia = [0]
\end{code}

\begin{code}
insertaDigito [] n = [n]
insertaDigito (x:xs) n = (x * 10 + n) : xs
\end{code}

\begin{code}
aplicaFuncion [] _ = []
aplicaFuncion xs@(x:[]) _ = xs
aplicaFuncion (x:y:xs) f = f y x : xs
\end{code}

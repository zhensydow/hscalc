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
module Controller( pulsaNumero, 
                   pulsaComa,
                   pulsaStackAdd, 
                   pulsaStackClear,
                   pulsaOpBinaria
                 ) where
\end{code}

\begin{code}
import Data.IORef
\end{code}

\begin{code}
import StackCalc( insertaDigito,
                  insertaComa,
                  pilaVacia,
                  nullValue,
                  convertValues,
                  aplicaFuncion )
import Vista( putStackInEntries )
\end{code}

\begin{code}
pulsaNumero v entries n = do
    val <- readIORef v
    let newVal = insertaDigito val n
    writeIORef v newVal
    putStackInEntries entries newVal
\end{code}

\begin{code}
pulsaComa v entries = do
    val <- readIORef v
    let newVal = insertaComa val
    writeIORef v newVal
    putStackInEntries entries newVal
\end{code}

\begin{code}
pulsaStackAdd v entries = do
    val <- readIORef v
    let newVal = nullValue:convertValues val
    writeIORef v newVal
    putStackInEntries entries newVal
\end{code}

\begin{code}
pulsaStackClear v entries = do
    writeIORef v pilaVacia
    putStackInEntries entries pilaVacia
\end{code}

\begin{code}
pulsaOpBinaria v entries f = do
    val <- readIORef v
    let newVal = aplicaFuncion val f
    writeIORef v newVal
    putStackInEntries entries newVal
\end{code}

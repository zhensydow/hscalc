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
module Controller( FuncionCalculadora,
                   pulsaNumero, 
                   pulsaComa,
                   pulsaSigno,
                   pulsaDelete,
                   pulsaStackAdd, 
                   pulsaStackPop,
                   pulsaStackDup,
                   pulsaStackClear,
                   pulsaOpBinaria
                 ) where
\end{code}

\begin{code}
import Data.IORef
\end{code}

\begin{code}
import Graphics.UI.Gtk
\end{code}

\begin{code}
import StackCalc( StackState,
                  BinaryOp,
                  insertaDigito,
                  insertaComa,
                  insertaSigno,
                  borraCaracter,
                  pilaVacia,
                  nullValue,
                  eliminaValue,
                  duplicaValue,
                  convertValues,
                  aplicaFuncion )
import Vista( putStackInEntries )
\end{code}

\begin{code}
type FuncionCalculadora = 
    IORef StackState -> [Entry] -> IO()
\end{code}

\begin{code}
pulsaFuncion :: 
    (StackState -> StackState) 
    -> IORef StackState -> [Entry] -> IO()
pulsaFuncion funcion v entries = do
   val <- readIORef v
   let newVal = funcion val
   writeIORef v newVal
   putStackInEntries entries newVal
\end{code}

\begin{code}
pulsaComa :: FuncionCalculadora
pulsaComa = pulsaFuncion insertaComa
\end{code}

\begin{code}
pulsaSigno :: FuncionCalculadora
pulsaSigno = pulsaFuncion insertaSigno
\end{code}

\begin{code}
pulsaDelete :: FuncionCalculadora
pulsaDelete = pulsaFuncion borraCaracter
\end{code}

\begin{code}
pulsaStackAdd :: FuncionCalculadora
pulsaStackAdd = pulsaFuncion 
                (\v-> nullValue:convertValues v)
\end{code}

\begin{code}
pulsaStackPop :: FuncionCalculadora
pulsaStackPop = pulsaFuncion eliminaValue
\end{code}

\begin{code}
pulsaStackDup :: FuncionCalculadora
pulsaStackDup = pulsaFuncion duplicaValue
\end{code}

\begin{code}
pulsaStackClear :: FuncionCalculadora
pulsaStackClear = pulsaFuncion (\_-> pilaVacia)
\end{code}

\begin{code}
pulsaNumero :: Integer -> FuncionCalculadora
pulsaNumero n = pulsaFuncion (\v-> insertaDigito v n)
\end{code}

\begin{code}
pulsaOpBinaria :: BinaryOp -> FuncionCalculadora
pulsaOpBinaria f = pulsaFuncion (\v-> aplicaFuncion v f)
\end{code}

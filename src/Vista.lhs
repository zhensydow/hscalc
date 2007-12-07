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
module Vista( putStackInEntries ) where
\end{code}

\begin{code}
import Graphics.UI.Gtk
\end{code}

\begin{code}
import StackCalc( StackState )
\end{code}

\begin{code}
putStackInEntries :: [Entry] -> StackState -> IO()
putStackInEntries [] _ = return ()
putStackInEntries (x:xs) [] = do
    set x [entryText := ""]
    putStackInEntries xs []
putStackInEntries (x:xs) (y:ys) = do
    set x [entryText := (show y)]
    putStackInEntries xs ys
\end{code}

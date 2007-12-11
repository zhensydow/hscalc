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
                  StackState,
                  BinaryOp,
                  stringToVal,
                  nullValue,
                  pilaVacia,
                  eliminaValue,
                  duplicaValue,
                  insertaDigito,
                  insertaComa,
                  insertaSigno,
                  borraCaracter,
                  convertValues,
                  aplicaFuncion,
                  aplicaFold
                ) where
\end{code}

\begin{code}
import Data.List( find )
\end{code}

\begin{code}
data StackValue = N Double |
                  T String
\end{code}

\begin{code}
type BinaryOp = Double -> Double -> Double
type StackState = [StackValue]
\end{code}

\begin{code}
instance Show StackValue where
    show (N v) = show v
    show (T v) = v
\end{code}

\begin{code}
stringToVal :: String -> StackValue
stringToVal s = T s
\end{code}

\begin{code}
nullValue :: StackValue
nullValue  = T "0"
\end{code}

\begin{code}
pilaVacia :: StackState
pilaVacia = [nullValue]
\end{code}

\begin{code}
eliminaValue :: StackState -> StackState
eliminaValue [] = pilaVacia
eliminaValue (_:[]) = pilaVacia
eliminaValue xs = tail xs
\end{code}

\begin{code}
duplicaValue :: StackState -> StackState
duplicaValue [] = pilaVacia
duplicaValue xss@(x:_) = convertValues $ x : xss
\end{code}

\begin{code}
extractDouble :: StackValue -> Double
extractDouble (N v) = v
extractDouble (T s)
    | (last s) == '.' = read (s++"0")
    | otherwise = read s
\end{code}

\begin{code}
convertValues :: StackState -> StackState
convertValues xs = map (\a-> N $ extractDouble a) xs
\end{code}

\begin{code}
insertaDigito :: StackState -> Integer -> StackState
insertaDigito [] n =  [T (show n)]
insertaDigito ((T "0"):xs) n = T (show n) : xs
insertaDigito ((T "-0"):xs) n = T ("-"++(show n)) : xs
insertaDigito ((T s):xs) n = T (s ++ show n) : xs
insertaDigito ((N 0.0):xs) n = T (show n) : xs
insertaDigito (x:xs) n = T (show n) : x : xs
\end{code}

\begin{code}
insertaComa :: StackState -> StackState
insertaComa xss@((T s):xs)
    | noComa = T (s ++ ".") : xs
    | otherwise = xss
    where noComa = Nothing == (find (=='.') s)
insertaComa xs = xs
\end{code}

\begin{code}
insertaSigno :: StackState -> StackState
insertaSigno ((T "0"):xs) = (T "0") : xs
insertaSigno ((T s):xs)
    | tieneSigno = (T $ tail s) : xs
    | otherwise = T ("-" ++ s) : xs
    where tieneSigno = '-' == (head s)
insertaSigno ((N 0.0):xs) = (N 0.0) : xs
insertaSigno ((N v):xs) = (N $ -v) : xs
insertaSigno xs = xs
\end{code}

\begin{code}
borraCaracter :: StackState -> StackState
borraCaracter [] = pilaVacia
borraCaracter ((T s):xs)
    | len > 2 = (T $ init s) : xs
    | sinSigno && len > 1 = (T $ init s) : xs
    | otherwise = nullValue : xs
    where sinSigno = not $ '-' == (head s)
          len = length s
borraCaracter ((N _):xs) = nullValue : xs
\end{code}

\begin{code}
aplicaFuncion :: StackState -> BinaryOp -> StackState
aplicaFuncion [] _ = []
aplicaFuncion (x:[]) _ = x:[]
aplicaFuncion (x:y:xs) f = N (f vy vx) : xs
    where vx = extractDouble x
          vy = extractDouble y
\end{code}

\begin{code}
aplicaFold :: StackState -> BinaryOp -> StackState
aplicaFold [] _ = []
aplicaFold xs f = [foldr1 ff $ convertValues xs]
    where ff x y = N $ f vy vx
              where vx = extractDouble x
                    vy = extractDouble y
\end{code}

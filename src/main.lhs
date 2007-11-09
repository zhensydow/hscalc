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
module Main where
\end{code}

\begin{code}
import Paths_hsCalc
\end{code}

\begin{code}
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
\end{code}

\begin{code}
import Data.IORef
\end{code}

\begin{code}
import StackCalc( insertaDigito, pilaVacia )
\end{code}

\begin{code}
pulsaNumero v entries n = do
    val <- readIORef v
    let newVal = insertaDigito val n
    writeIORef v newVal
    putStackInEntries entries newVal
\end{code}

\begin{code}
pulsaStackAdd v entries = do
    val <- readIORef v
    let newVal = 0:val
    writeIORef v newVal
    putStackInEntries entries newVal
\end{code}

\begin{code}
pulsaStackClear v entries = do
    writeIORef v pilaVacia
    putStackInEntries entries pilaVacia
\end{code}

\begin{code}
setNumButton dialog v entries n = do
    boton <- xmlGetWidget dialog castToButton $ "b_num_" ++ (show n)
    onClicked boton $ pulsaNumero v entries n
\end{code}

\begin{code}
putStackInEntries [] _ = return ()
putStackInEntries (x:xs) [] = do
    set x [entryText := ""]
    putStackInEntries xs []
putStackInEntries (x:xs) (y:ys) = do
    set x [entryText := (show y)]
    putStackInEntries xs ys
\end{code}

\begin{code}
main = do
    initGUI
    
    -- carga la especificacion
    name <- getDataFileName "data/window.glade"
    dialogXmlM <- xmlNew name
    let dialogXml = case dialogXmlM of
            (Just dialogXml) -> dialogXml
            Nothing -> error "can't find glade file"
    
    -- crea el valor por defecto
    value <- newIORef pilaVacia

    -- obten el campo donde se muestra el numero
    entry <- xmlGetWidget dialogXml castToEntry "e_num_0"

    -- crea la lista de entradas donde mostrar la pila
    entries <- mapM 
        (\a-> xmlGetWidget dialogXml castToEntry a) 
        ["e_num_"++(show x)|x<-[0..10]]

    -- poner valores por defecto
    putStackInEntries entries pilaVacia

    -- configura los botones numericos
    mapM (\n -> setNumButton dialogXml value entries n) [0..9]

    boton <- xmlGetWidget dialogXml castToButton "b_stack_add"
    onClicked boton $ pulsaStackAdd value entries
    
    boton <- xmlGetWidget dialogXml castToButton "b_stack_clear"
    onClicked boton $ pulsaStackClear value entries

    -- pon en pantalla la ventana
    window <- xmlGetWidget dialogXml castToWindow "window1"
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
\end{code}

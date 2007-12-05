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
import Controller( pulsaNumero, 
                   pulsaComa,
                   pulsaSigno,
                   pulsaStackAdd, 
                   pulsaStackClear,
                   pulsaOpBinaria )
import Vista( putStackInEntries )
import StackCalc( pilaVacia,
                  insertaComa )
\end{code}

\begin{code}
setNumButton dialog v entries n = do
    boton <- xmlGetWidget dialog castToButton $ name
    onClicked boton $ pulsaNumero v entries n
        where name = "b_num_" ++ (show n)
\end{code}

\begin{code}
setButton dialog name funcion = do
    boton <- xmlGetWidget dialog castToButton name
    onClicked boton funcion
\end{code}

\begin{code}
funciones = [
             ("b_stack_add",pulsaStackAdd),
             ("b_stack_clear", pulsaStackClear),
             ("b_coma", pulsaComa),
             ("b_signo", pulsaSigno)
            ]
\end{code}

\begin{code}
operaciones = [
               ("b_op_suma", (+)),
               ("b_op_mul", (*)),
               ("b_op_resta", (-)),
               ("b_op_div", (/))
              ]
\end{code}

\begin{code}
setupButtons dialog = do
    -- crea el valor por defecto
    value <- newIORef pilaVacia

    -- crea la lista de entradas donde mostrar la pila
    entries <- mapM 
        (\a-> xmlGetWidget dialog castToEntry a) 
        ["e_num_"++(show x)|x<-[0..10]]

    -- poner valores por defecto
    putStackInEntries entries pilaVacia

    mapM (\n -> setNumButton dialog value entries n) [0..9]

    -- configurar botones funcion simple
    mapM (\(n,f) -> 
              setButton dialog n $ f value entries )
         funciones

    -- configurar botones con operacion binaria
    mapM (\(n,f) -> 
              setButton dialog n $ pulsaOpBinaria value entries f )
         operaciones
\end{code}

\begin{code}
main = do
    initGUI
    
    -- carga la especificacion
    name <- getDataFileName "data/window.glade"
    dialogXmlM <- xmlNew name
    let dialogXml = case dialogXmlM of
            (Just d) -> d
            Nothing -> error "can't find glade file"
    
    -- configura los botones
    setupButtons dialogXml

    -- pon en pantalla la ventana
    window <- xmlGetWidget dialogXml castToWindow "window1"
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
\end{code}

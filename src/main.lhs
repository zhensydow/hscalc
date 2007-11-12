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
import Controller( 
                  pulsaNumero, 
                  pulsaStackAdd, 
                  pulsaStackClear,
                  pulsaOpBinaria )
import Vista( putStackInEntries )
import StackCalc( pilaVacia )
\end{code}

\begin{code}
setNumButton dialog v entries n = do
    boton <- xmlGetWidget dialog castToButton $ name
    onClicked boton $ pulsaNumero v entries n
        where name = "b_num_" ++ (show.toInteger.round) n
\end{code}

\begin{code}
setupButtons dialog value entries = do
    mapM (\n -> setNumButton dialog value entries n) [0..9]

    boton <- xmlGetWidget dialog castToButton "b_stack_add"
    onClicked boton $ pulsaStackAdd value entries
    
    boton <- xmlGetWidget dialog castToButton "b_stack_clear"
    onClicked boton $ pulsaStackClear value entries
    
    boton <- xmlGetWidget dialog castToButton "b_op_suma"
    onClicked boton $ pulsaOpBinaria value entries (+)

    boton <- xmlGetWidget dialog castToButton "b_op_mul"
    onClicked boton $ pulsaOpBinaria value entries (*)
              
    boton <- xmlGetWidget dialog castToButton "b_op_resta"
    onClicked boton $ pulsaOpBinaria value entries (-)

    boton <- xmlGetWidget dialog castToButton "b_op_div"
    onClicked boton $ pulsaOpBinaria value entries (/)
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

    -- crea la lista de entradas donde mostrar la pila
    entries <- mapM 
        (\a-> xmlGetWidget dialogXml castToEntry a) 
        ["e_num_"++(show x)|x<-[0..10]]

    -- configura los botones
    setupButtons dialogXml value entries

    -- poner valores por defecto
    putStackInEntries entries pilaVacia

    -- pon en pantalla la ventana
    window <- xmlGetWidget dialogXml castToWindow "window1"
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
\end{code}

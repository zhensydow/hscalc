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
import Paths_hscalc
\end{code}

\begin{code}
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
\end{code}

\begin{code}
import Data.IORef
\end{code}

\begin{code}
import Controller( FuncionCalculadora,
                   pulsaNumero, 
                   pulsaComa,
                   pulsaSigno,
                   pulsaDelete,
                   pulsaStackAdd, 
                   pulsaStackPop,
                   pulsaStackDup,
                   pulsaStackClear,
                   pulsaOpBinaria,
                   pulsaOpFold )
import Vista( putStackInEntries )
import StackCalc( StackState,
                  BinaryOp,
                  pilaVacia )
\end{code}

\begin{code}
setNumButton :: 
    GladeXML -> IORef StackState 
    -> [Entry] -> Integer -> IO()
setNumButton dialog v entries n = do
    boton <- xmlGetWidget dialog castToButton $ name
    onClicked boton $ pulsaNumero n v entries
    return ()
        where name = "b_num_" ++ (show n)
\end{code}

\begin{code}
setButton :: GladeXML -> String -> IO() -> IO()
setButton dialog name funcion = do
    boton <- xmlGetWidget dialog castToButton name
    onClicked boton funcion
    return ()
\end{code}

\begin{code}
muestraAbout :: Window -> IO()
muestraAbout parent = do
    -- carga la especificacion
    name <- getDataFileName "data/about.glade"
    aboutXmlM <- xmlNew name
    let aboutXml = case aboutXmlM of
            (Just d) -> d
            Nothing -> error "can't find glade file"

    aboutDialog <- xmlGetWidget aboutXml castToAboutDialog "about"

    -- Fija el nombre
    title <- windowGetTitle parent
    aboutDialogSetName aboutDialog title

    -- make the about dialog appear above the main window
    windowSetTransientFor aboutDialog parent

    -- make the dialog non-modal
    afterResponse aboutDialog $ \_ -> widgetDestroy aboutDialog
    widgetShow aboutDialog
    return ()
\end{code}

\begin{code}
funciones :: [( String, FuncionCalculadora )]
funciones = [
             ("b_stack_add",pulsaStackAdd),
             ("b_stack_pop",pulsaStackPop),
             ("b_stack_dup",pulsaStackDup),
             ("b_stack_clear", pulsaStackClear),
             ("b_coma", pulsaComa),
             ("b_signo", pulsaSigno),
             ("b_del", pulsaDelete)
            ]
\end{code}

\begin{code}
operaciones :: [( String, BinaryOp )]
operaciones = [
               ("b_op_suma", (+)),
               ("b_op_mul", (*)),
               ("b_op_resta", (-)),
               ("b_op_div", (/))
              ]
\end{code}

\begin{code}
operacionesFold :: [(String, BinaryOp)]
operacionesFold = [
                   ("b_opf_suma", (+)),
                   ("b_opf_mul", (*)),
                   ("b_opf_resta", (-)),
                   ("b_opf_div", (/))
                  ]
\end{code}

\begin{code}
getNumEntry :: GladeXML -> Integer -> IO Entry
getNumEntry dialog n = xmlGetWidget dialog castToEntry name
    where name = "e_num_" ++ (show n)
\end{code}

\begin{code}
setupButtons :: GladeXML -> IO()
setupButtons dialog = do
    -- crea el valor por defecto
    value <- newIORef pilaVacia

    -- crea la lista de entradas donde mostrar la pila
    entries <- mapM (getNumEntry dialog) [0..10]

    -- poner valores por defecto
    putStackInEntries entries pilaVacia

    mapM (\n -> setNumButton dialog value entries n) [0..9]

    -- configurar botones funcion simple
    mapM (\(n,f) -> 
              setButton dialog n $ f value entries )
         funciones

    -- configurar botones con operacion binaria
    mapM (\(n,f) -> 
              setButton dialog n $ 
              pulsaOpBinaria f value entries )
         operaciones

    -- configurar botones con operacion fold
    mapM (\(n,f) ->
              setButton dialog n $
              pulsaOpFold f value entries )
         operacionesFold

    window <- xmlGetWidget dialog castToWindow "main"

    setButton dialog "b_about" $ muestraAbout window

    return ()
\end{code}

\begin{code}
main :: IO()
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
    window <- xmlGetWidget dialogXml castToWindow "main"
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
\end{code}

module Lang where

data Lang = EN | DE | ES | SE | PD

localize :: Lang -> String -> String
localize lang = case lang of
  EN -> localizeEN
  DE -> localizeDE
  ES -> localizeES
  SE -> localizeSE
  PD -> localizePD
  _  -> localizeEN

localizeEN :: String -> String
localizeEN str = case str of
  "welcome"    -> "Welcome!"
  "helpAnno"   -> "Type \":h\" for help."
  "unknowncmd" -> "No such command found, type \":h\" for help."
  "loaded"     -> "Successfully loaded "
  "rule"       -> " rule(s)."
  "nofile"     -> "No file loaded yet."
  "nostrat"    -> "Successfully deselected strategy, subsequent queries will output the entire SLD tree."
  "strat"      -> "Successfully selected strategy '"
  "stratnotav" -> "No such strategy available! Try one of these: "
  "goodbye"    -> "Goodbye!"
  "cmdsav"     -> "Commands available from the prompt"
  _            -> str

localizeDE :: String -> String
localizeDE str = case str of
  "welcome"    -> "Willkommen"
  "helpAnno"   -> "Drücken Sie \":h\" für Hilfe."
  "unknowncmd" -> "Dieser Befehl ist nicht verfügbar, Drücken Sie \":h\" für Hilfe."
  "loaded"     -> "Erfolgreiches Laden von "
  "rule"       -> " Regel(n)."
  "nofile"     -> "Keine Datei geladen."
  "nostrat"    -> "Die Suchstrategie wurde erfolgreich entfernt. Es wird jetzt der SLD-Baum ausgegeben."
  "strat"      -> "Folgende Suchstrategie wurde ausgewählt '"
  "stratnotav" -> "Gewünschte Strategie ist nicht verfügbar! Verfügbare Strategien: "
  "goodbye"    -> "Auf Wiedersehen!"
  "cmdsav"     -> "Verfügbare Befehle"
  _            -> str

localizeES :: String -> String
localizeES str = case str of
  "welcome"    -> "Bienvenido"
  "helpAnno"   -> "Tocar \":h\" por ayuda."
  "unknowncmd" -> "No hay este instrucción, tocar \":h\" por ayuda."
  "loaded"     -> "Carcar "
  "rule"       -> " regla(s) con exito."
  "nofile"     -> "No carpeta."
  "nostrat"    -> "La estrategia esta removado con exito. El arbol SLD esta emitiado."
  "strat"      -> "Seleccionado la estrategia consiguiente '"
  "stratnotav" -> "La estrategia deseada no es disponible! Estrategias disponibles: "
  "goodbye"    -> "Adios!"
  "cmdsav"     -> "Instrucciónes disponibles"
  _            -> str

localizePD :: String -> String
localizePD str = case str of
  "welcome"    -> "Moin!"
  "helpAnno"   -> "Tipp \":h\" för Help."
  "unknowncmd" -> "Dat will nich bottern, Tipp \":h\" för Help."
  "loaded"     -> "Düchtiges rinladen af "
  "rule"       -> " Gebode(n)."
  "nofile"     -> "Dat gifft keene Datei."
  "nostrat"    -> "Die Vorgahn ward wegmakt. Ick giff di nu den SLD-Boom."
  "strat"      -> "Folgende Suchstrategie wurde ausgewählt '"
  "stratnotav" -> "Dat gifft disse Vörgahn nich! Paraate Vörgahn: "
  "goodbye"    -> "Tschüüs!"
  "cmdsav"     -> "Paraate Anwiesen"
  _            -> str

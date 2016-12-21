module App.Effects where

import Node.Buffer (BUFFER)
import Node.FS (FS)

type AppEffects e = (buffer :: BUFFER, fs :: FS | e)

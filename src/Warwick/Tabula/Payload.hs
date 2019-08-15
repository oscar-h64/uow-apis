--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

-- | This module re-exports modules containing payload data types.
module Warwick.Tabula.Payload (
    module Payload
) where

--------------------------------------------------------------------------------

import Warwick.Tabula.Payload.CourseDetails as Payload
import Warwick.Tabula.Payload.Department as Payload
import Warwick.Tabula.Payload.Holiday as Payload
import Warwick.Tabula.Payload.Module as Payload
import Warwick.Tabula.Payload.StudentCourseDetails as Payload
import Warwick.Tabula.Payload.Term as Payload
import Warwick.Tabula.Payload.Week as Payload

--------------------------------------------------------------------------------

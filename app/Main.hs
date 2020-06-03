-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Main ( main ) where 

--------------------------------------------------------------------------------

import CmdArgs
import Sitebuilder

-------------------------------------------------------------------------------

-- | 'main' is the main entry point for this application.
main :: IO ()
main = do
    args <- parseCmdLineArgs

    case args of 
        SitebuilderCmd opts -> sitebuilderMain opts

-------------------------------------------------------------------------------
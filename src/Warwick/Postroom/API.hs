-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Postroom.API where 

-------------------------------------------------------------------------------

-- import Data.Text ( Text )
import Data.Proxy

import Servant.API
import Servant.Client

import Warwick.Postroom.OpeningTimes ( OpeningTimes )
import Warwick.Postroom.PostItem ( PostItem )
import Warwick.Postroom.PostroomHub ( PostroomHub )
import Warwick.Postroom.Recipient ( Recipient )

--------------------------------------------------------------------------------

type PostroomAuth = BasicAuth "" ()

type PostroomApi =
      -- GET /api/hubs
      -- Gets all the postroom hubs currently on campus
      PostroomAuth :>
      "hubs" :> 
      Get '[JSON] [PostroomHub]
      -- GET /api/opening-times
      -- Gets the opening times of the postroom hubs and a list of which
      -- accommodations are linked to each hub
 :<|> PostroomAuth :>
      "opening-times" :>
      Get '[JSON] OpeningTimes
      -- GET /api/my-address
      -- Gets active addresses for the current user
 :<|> PostroomAuth :>
      "my-address" :>
      Get '[JSON] [Recipient]
      -- GET /api/my-items
      -- Gets all post items for the current user
 :<|> PostroomAuth :>
      "my-items" :>
      Get '[JSON] [PostItem]

-- | A proxy value for the `API` type.
postroom :: Proxy PostroomApi
postroom = Proxy

--------------------------------------------------------------------------------

getHubs :: 
    BasicAuthData -> 
    ClientM [PostroomHub]

getOpeningTimes ::
    BasicAuthData ->
    ClientM OpeningTimes

getMyAddress :: 
    BasicAuthData -> 
    ClientM [Recipient]

getMyItems ::
    BasicAuthData ->
    ClientM [PostItem]

getHubs :<|> getOpeningTimes :<|> getMyAddress :<|> getMyItems = client postroom

--------------------------------------------------------------------------------

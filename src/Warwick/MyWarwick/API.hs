-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.MyWarwick.API (
    MyWarwick,
    postAlert,
    postActivity
) where 

--------------------------------------------------------------------------------

import Data.Text
import Data.Proxy

import Servant.API
import Servant.Client

import Warwick.MyWarwick.Response
import Warwick.MyWarwick.StreamItem

--------------------------------------------------------------------------------

type MyWarwickAuth = BasicAuth "" ()

type MyWarwick =
      MyWarwickAuth :>
      "streams" :> 
      Capture "provider" Text :> 
      "alerts" :>
      ReqBody '[JSON] StreamItem :>
      Post '[JSON] MyWarwickResponse
 :<|> MyWarwickAuth :>
      "streams" :> 
      Capture "provider" Text :> 
      "activities" :>
      ReqBody '[JSON] StreamItem :>
      Post '[JSON] MyWarwickResponse

-- | A proxy value for the `MyWarwick` type.
mywarwick :: Proxy MyWarwick
mywarwick = Proxy

--------------------------------------------------------------------------------

postAlert :: 
    BasicAuthData ->
    Text -> 
    StreamItem ->
    ClientM MyWarwickResponse

postActivity :: 
    BasicAuthData ->
    Text -> 
    StreamItem ->
    ClientM MyWarwickResponse


postAlert :<|> postActivity = client mywarwick

--------------------------------------------------------------------------------
    
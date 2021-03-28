-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.AEP.API (
    AEP,
    FileUpload(..),
    uploadFile,
) where 

--------------------------------------------------------------------------------

import qualified Data.HashMap.Lazy as HM
import Data.Text
import Data.Proxy
import Data.UUID

import Servant.API
import Servant.Client

import Web.FormUrlEncoded

-------------------------------------------------------------------------------

data FileUpload = MkFileUpload {
    fuCsrf :: Text
}

instance ToForm FileUpload where
    toForm MkFileUpload{..} = Form $ HM.singleton "csrfToken" [fuCsrf]

-------------------------------------------------------------------------------

type AEPAuth = Header "Cookie" Text

type AEP =
      AEPAuth :>
      "assessment" :>
      Capture "assessmentID" UUID :>
      "upload" :>
      ReqBody '[FormUrlEncoded] FileUpload :>
      Post '[JSON] () 

aep :: Proxy AEP
aep = Proxy

--------------------------------------------------------------------------------

uploadFile :: 
    Maybe Text ->
    UUID ->
    FileUpload ->
    ClientM ()

uploadFile = client aep

--------------------------------------------------------------------------------
    
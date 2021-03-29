-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.AEP.API (
    AEP,
    uploadFile
) where 

--------------------------------------------------------------------------------

import Data.Text
import Data.Proxy
import Data.UUID

import Servant.API
import Servant.Client

import Warwick.AEP.FileUpload
import Warwick.MultiPart

-------------------------------------------------------------------------------

type AEPAuth = Header "Cookie" Text

type AEP =
      AEPAuth :>
      "assessment" :>
      Capture "assessmentID" UUID :>
      "upload" :>
      Header "OnlineExams-Upload" Bool :>
      ReqBody '[MultiPart] FileUpload :>
      Post '[JSON] () 

aep :: Proxy AEP
aep = Proxy

--------------------------------------------------------------------------------

uploadFile :: 
    Maybe Text ->
    UUID ->
    Maybe Bool ->
    FileUpload ->
    ClientM ()

uploadFile = client aep

--------------------------------------------------------------------------------
    
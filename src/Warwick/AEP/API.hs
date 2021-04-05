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

import Data.Proxy
import Data.Text
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
      Header' '[Required, Strict] "OnlineExams-Upload" Bool :>
      Header' '[Required, Strict] "User-Agent" Text :>
      ReqBody '[MultiPart] FileUpload :>
      Post '[*] NoContent

aep :: Proxy AEP
aep = Proxy

--------------------------------------------------------------------------------

uploadFile ::
    Maybe Text ->
    UUID ->
    Bool ->
    Text ->
    FileUpload ->
    ClientM NoContent

uploadFile = client aep

--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiSandbox (UserAPI) where

import Servant
import Servant.API
import Data.Time (UTCTime)

-- Experiments below --

type UserAPI = "users" :> QueryParam "sortBy" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User {
  name :: String,
  age :: Int,
  email :: String, 
  registration_date :: UTCTime
}
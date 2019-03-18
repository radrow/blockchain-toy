module Crypto where

import Codec.Crypto.RSA( generateKeyPair
                       , encrypt, decrypt
                       )
import Data.Hashable.Generic

import Types


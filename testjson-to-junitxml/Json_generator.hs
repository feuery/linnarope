{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json_generator where
import TextShow

generator min max =
  show [min..max]

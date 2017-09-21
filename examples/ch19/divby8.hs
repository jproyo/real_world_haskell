{-- snippet all --}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DatatypeContexts #-}

import Control.Monad.Error

data Show a => 
    DivByError a = DivBy0
                  | ForbiddenDenominator a
                  | OtherDivByError String
                    deriving (Eq, Read, Show)

instance Error (DivByError a) where
    strMsg x = OtherDivByError x

divBy :: (Show a, Integral a) => a -> [a] -> Either (DivByError a) [a]
divBy = divByGeneric

divByGeneric :: (Show a, Integral a, MonadError (DivByError a) m) =>
                 a -> [a] -> m [a]
divByGeneric _ [] = return []
divByGeneric _ (0:_) = throwError DivBy0
divByGeneric _ (10:_) = throwError (ForbiddenDenominator 10)
divByGeneric _ (20:_) = throwError (ForbiddenDenominator 20)
divByGeneric numerator (denom:xs) =
    do next <- divByGeneric numerator xs
       return ((numerator `div` denom) : next)
{-- /snippet all --}

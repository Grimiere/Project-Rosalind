{-# LANGUAGE OverloadedStrings #-} --Needed for HTTP client.

module Uniprot (
    idToPeptideFASTA
) where

import FASTA
import Peptide
import Network.HTTP.Simple
import Control.Monad
import qualified Data.ByteString.Char8 as B8

idToPeptideFASTA :: String -> IO (Maybe (FASTA Peptide))
idToPeptideFASTA [] = return $ Nothing
idToPeptideFASTA xs = do
    let req = idToRequest xs
    case req of
        Nothing -> return $ Nothing
        Just gReq -> do  
            body <- B8.unpack <$> getResponseBody <$> httpBS gReq
            return $ head $ stringToPeptideFASTAS body

idToRequest :: String -> Maybe Request
idToRequest xs = parseRequest $ "https://www.uniprot.org/uniprot/" ++ xs ++ ".fasta"
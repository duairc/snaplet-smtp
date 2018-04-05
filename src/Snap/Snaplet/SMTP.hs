{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.SMTP
    ( SMTP, initSMTP, mail
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Word (Word16)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Prelude hiding (lookup)


-- configurator --------------------------------------------------------------
import           Data.Configurator (lookup, lookupDefault)
import qualified Data.Configurator.Types as C


-- filepath ------------------------------------------------------------------
import           System.FilePath ((</>))


-- mime-mail -----------------------------------------------------------------
import           Network.Mail.Mime (Mail)


-- mtl -----------------------------------------------------------------------
import           Control.Monad.Reader.Class (MonadReader, ask)


-- network -------------------------------------------------------------------
import           Network.Socket (PortNumber)


-- resource-pool -------------------------------------------------------------
import           Data.Pool (Pool, createPool, withResource)


-- smtp-mail -----------------------------------------------------------------
import qualified Network.Mail.SMTP as M


-- snap ----------------------------------------------------------------------
import           Snap.Snaplet (SnapletInit, makeSnaplet, getSnapletUserConfig)


-- snaplet-smtp --------------------------------------------------------------
import           Paths_snaplet_smtp (getDataDir)


------------------------------------------------------------------------------
data SMTPConfig = SMTPConfig
    { _hostname :: !String
    , _port :: !PortNumber
    , _username :: !(Maybe String)
    , _password :: !(Maybe String)
    }


------------------------------------------------------------------------------
mkSMTPConfig :: C.Config -> IO SMTPConfig
mkSMTPConfig config = SMTPConfig
    <$> lookupDefault "localhost" config "hostname"
    <*> (fromIntegral <$> lookupDefault (25 :: Word16) config "port")
    <*> lookup config "username"
    <*> lookup config "password"


------------------------------------------------------------------------------
configDir :: Maybe (IO FilePath)
configDir = Just $ (</> "config") <$> getDataDir


------------------------------------------------------------------------------
data SMTP = SMTP
    { _pool :: !(Pool M.SMTPConnection)
    }
  deriving (Generic, Typeable)


------------------------------------------------------------------------------
loadSMTP :: SMTPConfig -> IO SMTP
loadSMTP config = SMTP <$> createPool connect M.closeSMTP 1 60 20
  where
    SMTPConfig hostname port musername mpassword = config
    connect = do
        connection <- M.connectSMTP' hostname port
        case (musername, mpassword) of
            (Just username, Just password) -> do
                _ <- M.login connection username password
                pure connection
            _ -> pure connection


------------------------------------------------------------------------------
initSMTP :: SnapletInit b SMTP
initSMTP = makeSnaplet "smtp" "SMTP mail sender snaplet" configDir $
    getSnapletUserConfig >>= liftIO . mkSMTPConfig >>= liftIO . loadSMTP


------------------------------------------------------------------------------
mail :: (MonadReader SMTP m, MonadIO m) => Mail -> m ()
mail message = do
    SMTP pool <- ask
    liftIO $ withResource pool (flip M.renderAndSend message)

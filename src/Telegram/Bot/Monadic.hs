{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use writeList2Chan" #-}

module Telegram.Bot.Monadic where

import Control.Applicative ( Alternative((<|>)) )
import Control.Concurrent.Async ( poll, async )
import Control.Concurrent.Chan ( writeChan, newChan, Chan )
import Control.Monad (foldM)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Loops (iterateM_)
import Data.Map ( lookup, insert, empty )
import Data.Text ( Text )
import Safe (lastMay)
import Servant.Client ( ClientM, ClientError )
import System.IO (hPutStrLn, stderr)
import Telegram.Bot.API as API
    ( toSendDocument,
      updateChatId,
      getUpdates,
      defaultRunBot,
      EditMessageReplyMarkupRequest(EditMessageReplyMarkupRequest),
      EditMessageMediaRequest(EditMessageMediaRequest),
      EditMessageCaptionRequest(EditMessageCaptionRequest),
      EditMessageTextRequest(EditMessageTextRequest),
      CallbackQueryId,
      AnswerCallbackQueryRequest(AnswerCallbackQueryRequest),
      InputMedia,
      SendMediaGroupRequest(SendMediaGroupRequest),
      SendVideoNoteRequest(SendVideoNoteRequest),
      SendVoiceRequest(SendVoiceRequest),
      SendAnimationRequest(SendAnimationRequest),
      SendVideoRequest(SendVideoRequest),
      InputFile,
      SendAudioRequest(SendAudioRequest),
      PhotoFile,
      SendPhotoRequest(SendPhotoRequest),
      DocumentFile,
      SendDocumentRequest,
      CopyMessageRequest(CopyMessageRequest),
      ForwardMessageRequest(ForwardMessageRequest),
      SomeReplyMarkup,
      SomeChatId(SomeChatId),
      SendMessageRequest(SendMessageRequest),
      MessageId,
      CallbackQuery,
      Message,
      ChatId(..),
      GetUpdatesRequest(GetUpdatesRequest),
      Response(responseResult),
      Update(updateUpdateId, updateMessage, updateEditedMessage,
             updateChannelPost, updateEditedChannelPost, updateInlineQuery,
             updateChosenInlineResult, updateCallbackQuery, updateShippingQuery,
             updatePreCheckoutQuery),
      UpdateId(UpdateId),
      Token,
      ChosenInlineResult,
      InlineQuery,
      PreCheckoutQuery,
      ShippingQuery, ReplyKeyboardMarkup (..), KeyboardButton (..) )
import Control.Concurrent (readChan)
import Control.Exception (IOException, catch)
import Control.Monad.Except (MonadError(catchError))

-- type TelegramInteraction a = ReaderT (ChatId, Chan Telegram.Bot.Monadic.Update) ClientM a

data ChatChannel = ChatChannel {channelChatId :: ChatId, channelUpdateChannel :: Chan SomeUpdate}

runTelegramIntegrationBot :: MonadIO m => Token -> (ChatChannel -> ClientM a) -> m (Either ClientError a)
runTelegramIntegrationBot token interaction = do
  liftIO $ defaultRunBot token $ do
    flip iterateM_ (Nothing, Data.Map.empty) $ \(i, sessions) -> do
      updates <- catchError (responseResult <$> getUpdates (GetUpdatesRequest (fmap (\(UpdateId n) -> UpdateId (n + 1)) i) Nothing (Just 60) (Just []))) (const $ pure [])
      sessions' <-
        foldM
          ( \sessionsAcc upd -> do
              case updateChatId upd of
                Just (ChatId chatId) ->
                  liftIO $
                    let newSession = do
                          hPutStrLn stderr $ "Creating new session for chat " ++ show chatId
                          session <- newChan
                          mapM_ (writeChan session) (selectUpdate upd)
                          task <- async $ catch (defaultRunBot token $ interaction $ ChatChannel (ChatId chatId) session) (\e -> fail $ show (e :: IOException))
                          pure (insert chatId (session, task) sessionsAcc)
                     in case Data.Map.lookup chatId sessionsAcc of
                          Just (session, task) -> do
                            poll task >>= \case
                              Just _ -> newSession
                              Nothing -> do
                                mapM_ (writeChan session) (selectUpdate upd)
                                pure sessionsAcc
                          Nothing -> newSession
                Nothing -> pure sessionsAcc
          )
          sessions
          updates
      pure (lastMay (updateUpdateId <$> updates), sessions')

data SomeUpdate
  = SomeNewMessage Message
  | SomeEditedMessage Message
  | SomeNewChannelPost Message
  | SomeEditedChannelPost Message
  | SomeNewInlineQuery InlineQuery
  | SomeNewChosenInlineResult ChosenInlineResult
  | SomeNewCallbackQuery CallbackQuery
  | SomeNewShippingQuery ShippingQuery
  | SomeNewPreCheckoutQuery PreCheckoutQuery

selectUpdate :: Update -> Maybe SomeUpdate
selectUpdate upd =
  SomeNewMessage <$> updateMessage upd
    <|> SomeEditedMessage <$> updateEditedMessage upd
    <|> SomeNewChannelPost <$> updateChannelPost upd
    <|> SomeEditedChannelPost <$> updateEditedChannelPost upd
    <|> SomeNewInlineQuery <$> updateInlineQuery upd
    <|> SomeNewChosenInlineResult <$> updateChosenInlineResult upd
    <|> SomeNewCallbackQuery <$> updateCallbackQuery upd
    <|> SomeNewShippingQuery <$> updateShippingQuery upd
    <|> SomeNewPreCheckoutQuery <$> updatePreCheckoutQuery upd

untilRight :: Monad m => m (Either a b) -> (a -> m ()) -> m b
untilRight action fallback = action >>= \case
  Left a -> fallback a >> untilRight action fallback
  Right b -> pure b

getUpdate :: Chan SomeUpdate -> ClientM SomeUpdate
getUpdate = liftIO . readChan

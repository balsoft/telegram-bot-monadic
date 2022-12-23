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
      ShippingQuery )

-- type TelegramInteraction a = ReaderT (ChatId, Chan Telegram.Bot.Monadic.Update) ClientM a

data ChatChannel = ChatChannel {_chatId :: ChatId, updateChannel :: Chan SomeUpdate}

runTelegramIntegrationBot :: Token -> (ChatChannel -> ClientM a) -> IO (Either ClientError a)
runTelegramIntegrationBot token interaction = do
  defaultRunBot token $ do
    flip iterateM_ (Nothing, Data.Map.empty) $ \(i, sessions) -> do
      updates <- getUpdates $ GetUpdatesRequest (fmap (\(UpdateId n) -> UpdateId (n + 1)) i) Nothing (Just 1) (Just [])
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
                          task <- async $ defaultRunBot token $ interaction $ ChatChannel (ChatId chatId) session
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
          $ responseResult updates
      pure (lastMay (updateUpdateId <$> responseResult updates), sessions')

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

sendMessageRequest :: ChatId -> Text -> SendMessageRequest
sendMessageRequest c t = SendMessageRequest (SomeChatId c) t Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

forwardMessageRequest :: ChatId -> ChatId -> MessageId -> ForwardMessageRequest
forwardMessageRequest cid cid' = ForwardMessageRequest (SomeChatId cid) (SomeChatId cid') Nothing Nothing

copyMessageRequest :: ChatId -> ChatId -> MessageId -> CopyMessageRequest
copyMessageRequest cid cid' mid = CopyMessageRequest (SomeChatId cid) (SomeChatId cid') mid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

sendDocumentRequest :: ChatId -> DocumentFile -> SendDocumentRequest
sendDocumentRequest cid = API.toSendDocument (SomeChatId cid)

sendPhotoRequest :: ChatId -> PhotoFile -> SendPhotoRequest
sendPhotoRequest cid pf = SendPhotoRequest (SomeChatId cid) pf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

sendAudioRequest :: ChatId -> InputFile -> SendAudioRequest
sendAudioRequest cid pf = SendAudioRequest (SomeChatId cid) pf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

sendVideoRequest :: ChatId -> InputFile -> SendVideoRequest
sendVideoRequest cid pf =SendVideoRequest (SomeChatId cid) pf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

sendAnimationRequest :: ChatId -> InputFile -> SendAnimationRequest
sendAnimationRequest cid pf = SendAnimationRequest (SomeChatId cid) pf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

sendVoiceRequest :: ChatId -> InputFile -> SendVoiceRequest
sendVoiceRequest cid pf = SendVoiceRequest (SomeChatId cid) pf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

sendVideoNoteRequest :: ChatId -> InputFile -> SendVideoNoteRequest
sendVideoNoteRequest cid pf = SendVideoNoteRequest (SomeChatId cid) pf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

sendMediaGroupRequest :: ChatId -> [InputMedia] -> SendMediaGroupRequest
sendMediaGroupRequest cid pf = SendMediaGroupRequest (SomeChatId cid) pf Nothing Nothing Nothing Nothing Nothing

answerCallbackQueryRequest :: CallbackQueryId -> AnswerCallbackQueryRequest
answerCallbackQueryRequest qid = AnswerCallbackQueryRequest qid Nothing Nothing Nothing Nothing

editMessageTextRequest :: Text -> EditMessageTextRequest
editMessageTextRequest t = EditMessageTextRequest Nothing Nothing Nothing t Nothing Nothing Nothing Nothing

editMessageCaptionRequest :: EditMessageCaptionRequest
editMessageCaptionRequest = EditMessageCaptionRequest Nothing Nothing Nothing Nothing Nothing Nothing Nothing

editMessageMediaRequest :: InputMedia -> EditMessageMediaRequest
editMessageMediaRequest m = EditMessageMediaRequest Nothing Nothing Nothing m Nothing

editMessageReplyMarkupRequest :: Maybe SomeReplyMarkup -> EditMessageReplyMarkupRequest
editMessageReplyMarkupRequest = EditMessageReplyMarkupRequest Nothing Nothing Nothing

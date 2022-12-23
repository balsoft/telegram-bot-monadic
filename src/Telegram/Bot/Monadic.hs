{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Bot.Monadic where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad (foldM, foldM_, forever)
import Control.Monad.IO.Class
import Control.Monad.Loops (iterateM_)
import Control.Monad.Trans.State
import Data.Foldable (forM_)
import Data.Functor
import Data.Map
import Data.Text
import Safe (lastMay)
import Servant.Client hiding (Response)
import System.IO (hPutStrLn, stderr)
import Telegram.Bot.API
import Text.ParserCombinators.ReadPrec (Prec)

type TelegramInteraction a = ReaderT (ChatId, Chan Update) ClientM a

runTelegramIntegrationBot :: Token -> TelegramInteraction () -> IO (Either ClientError ())
runTelegramIntegrationBot token interaction = do
  defaultRunBot token $ do
    flip iterateM_ (Nothing, Data.Map.empty) $ \(i, sessions) -> do
      updates <- getUpdates $ GetUpdatesRequest (fmap (\(UpdateId n) -> UpdateId (n + 1)) i) Nothing (Just 1) (Just [])
      sessions' <-
        foldM
          ( \sessions upd -> do
              case updateChatId upd of
                Just (ChatId chatId) ->
                  liftIO $
                    let newSession = do
                          hPutStrLn stderr $ "Creating new session for chat " ++ show chatId
                          session <- newChan
                          writeChan session upd
                          task <- async $ defaultRunBot token (runReaderT interaction (ChatId chatId, session))
                          pure (insert chatId (session, task) sessions)
                     in case Data.Map.lookup chatId sessions of
                          Just (session, task) -> do
                            poll task >>= \case
                              Just smth -> print smth >> newSession
                              Nothing -> do
                                writeChan session upd
                                pure sessions
                          Nothing -> newSession
                Nothing -> pure sessions
          )
          sessions
          $ responseResult updates
      pure (lastMay (updateUpdateId <$> responseResult updates), sessions')

getChatId :: TelegramInteraction ChatId
getChatId = fst <$> ask

-- data Update = NewMessage Message | EditedMessage Message | NewChannelPost Message | EditedChannelPost Message | NewInlineQuery InlineQuery | NewChosenInlineResult ChosenInlineResult | NewCallbackQuery CallbackQuery | NewShippingQuery ShippingQuery | NewPreCheckoutQuery PreCheckoutQuery

-- selectUpdate :: Telegram.Bot.API.Update -> Maybe Telegram.Bot.Monadic.Update
-- selectUpdate upd = NewMessage <$> updateMessage upd <|> EditedMessage <$> updateEditedMessage upd <|> NewChannelPost <$> updateChannelPost upd <|> EditedChannelPost <$> updateEditedChannelPost upd <|> NewInlineQuery <$> updateInlineQuery upd <|> NewChosenInlineResult <$> updateChosenInlineResult upd <|> NewCallbackQuery <$> updateCallbackQuery upd <|> NewShippingQuery <$> updateShippingQuery upd <|> NewPreCheckoutQuery <$> updatePreCheckoutQuery upd

getUpdate :: TelegramInteraction Update
getUpdate = ask >>= \(_, chan) -> liftIO $ readChan chan

getNewMessage :: TelegramInteraction Message
getNewMessage =
  getUpdate >>= \case
    Update {updateMessage = Just msg} -> pure msg
    _ -> getNewMessage

getNewTextMessage :: TelegramInteraction Message
getNewTextMessage =
  getUpdate >>= \case
    Update {updateMessage = Just msg@(Message {messageText = Just _})} -> pure msg
    _ -> getNewTextMessage

getEditedMessage :: TelegramInteraction Message
getEditedMessage =
  getUpdate >>= \case
    Update {updateEditedMessage = Just msg} -> pure msg
    _ -> getEditedMessage

getCallbackQuery :: TelegramInteraction CallbackQuery
getCallbackQuery =
  getUpdate >>= \case
    Update {updateCallbackQuery = Just q} -> pure q
    _ -> getCallbackQuery

getCallbackQueryFor :: MessageId -> TelegramInteraction CallbackQuery
getCallbackQueryFor msg =
  getCallbackQuery >>= \q -> case q of
    CallbackQuery {callbackQueryMessage = Just msg'} | messageMessageId msg' == msg -> pure q
    _ -> getCallbackQueryFor msg

getMe :: TelegramInteraction (Response User)
getMe = ReaderT $ const Telegram.Bot.API.getMe

deleteMessage :: ChatId -> MessageId -> TelegramInteraction (Response Bool)
deleteMessage chatId = ReaderT . const . Telegram.Bot.API.deleteMessage chatId

deleteMessageHere :: MessageId -> TelegramInteraction (Response Bool)
deleteMessageHere mid = getChatId >>= \cid -> ReaderT $ const $ Telegram.Bot.API.deleteMessage cid mid

sendMessage :: SendMessageRequest -> TelegramInteraction (Response Message)
sendMessage = ReaderT . const . Telegram.Bot.API.sendMessage

toSendMessageHere :: Text -> TelegramInteraction SendMessageRequest
toSendMessageHere t = getChatId >>= \cid -> ReaderT $ const $ pure $ SendMessageRequest (SomeChatId cid) t Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

sendMessageHere :: Text -> TelegramInteraction (Response Message)
sendMessageHere t = toSendMessageHere t >>= Telegram.Bot.Monadic.sendMessage

forwardMessage :: ForwardMessageRequest -> TelegramInteraction (Response Message)
forwardMessage = ReaderT . const . Telegram.Bot.API.forwardMessage

toForwardMessageToHere :: SomeChatId -> MessageId -> TelegramInteraction ForwardMessageRequest
toForwardMessageToHere cid' mid = getChatId >>= \cid -> ReaderT $ const $ pure $ ForwardMessageRequest (SomeChatId cid) cid' Nothing Nothing mid

toForwardMessageFromHere :: SomeChatId -> MessageId -> TelegramInteraction ForwardMessageRequest
toForwardMessageFromHere cid' mid = getChatId >>= \cid -> ReaderT $ const $ pure $ ForwardMessageRequest cid' (SomeChatId cid) Nothing Nothing mid

forwardMessageToHere :: SomeChatId -> MessageId -> TelegramInteraction (Response Message)
forwardMessageToHere cid' mid = toForwardMessageToHere cid' mid >>= Telegram.Bot.Monadic.forwardMessage

forwardMessageFromHere :: SomeChatId -> MessageId -> TelegramInteraction (Response Message)
forwardMessageFromHere cid' mid = toForwardMessageFromHere cid' mid >>= Telegram.Bot.Monadic.forwardMessage

copyMessage :: CopyMessageRequest -> TelegramInteraction (Response CopyMessageId)
copyMessage = ReaderT . const . Telegram.Bot.API.copyMessage

toCopyMessageToHere :: SomeChatId -> MessageId -> TelegramInteraction CopyMessageRequest
toCopyMessageToHere cid' mid = getChatId >>= \cid -> ReaderT $ const $ pure $ CopyMessageRequest (SomeChatId cid) cid' mid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

toCopyMessageFromHere :: SomeChatId -> MessageId -> TelegramInteraction CopyMessageRequest
toCopyMessageFromHere cid' mid = getChatId >>= \cid -> ReaderT $ const $ pure $ CopyMessageRequest cid' (SomeChatId cid) mid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

copyMessageToHere :: SomeChatId -> MessageId -> TelegramInteraction (Response CopyMessageId)
copyMessageToHere cid' mid = toCopyMessageToHere cid' mid >>= Telegram.Bot.Monadic.copyMessage

copyMessageFromHere :: SomeChatId -> MessageId -> TelegramInteraction (Response CopyMessageId)
copyMessageFromHere cid' mid = toCopyMessageFromHere cid' mid >>= Telegram.Bot.Monadic.copyMessage

sendDocument :: SendDocumentRequest -> ReaderT r ClientM (Response Message)
sendDocument sdr = ReaderT $ const $ Telegram.Bot.API.sendDocument sdr

toSendDocumentHere :: DocumentFile -> TelegramInteraction SendDocumentRequest
toSendDocumentHere df = getChatId >>= \cid -> ReaderT $ const $ pure $ Telegram.Bot.API.toSendDocument (SomeChatId cid) df

sendDocumentHere :: DocumentFile -> TelegramInteraction (Response Message)
sendDocumentHere df = toSendDocumentHere df >>= Telegram.Bot.Monadic.sendDocument

getFile :: FileId -> TelegramInteraction (Response File)
getFile = ReaderT . const . Telegram.Bot.API.getFile

sendPhoto :: SendPhotoRequest -> TelegramInteraction (Response Message)
sendPhoto = ReaderT . const . Telegram.Bot.API.sendPhoto

toSendPhotoHere :: PhotoFile -> TelegramInteraction SendPhotoRequest
toSendPhotoHere pf = getChatId >>= \cid -> ReaderT $ const $ pure $ SendPhotoRequest (SomeChatId cid) pf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

sendPhotoHere :: PhotoFile -> TelegramInteraction (Response Message)
sendPhotoHere t = toSendPhotoHere t >>= Telegram.Bot.Monadic.sendPhoto

sendAudio :: SendAudioRequest -> TelegramInteraction (Response Message)
sendAudio = ReaderT . const . Telegram.Bot.API.sendAudio

toSendAudioHere :: InputFile -> TelegramInteraction SendAudioRequest
toSendAudioHere pf = getChatId >>= \cid -> ReaderT $ const $ pure $ SendAudioRequest (SomeChatId cid) pf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

sendAudioHere :: InputFile -> TelegramInteraction (Response Message)
sendAudioHere t = toSendAudioHere t >>= Telegram.Bot.Monadic.sendAudio

sendVideo :: SendVideoRequest -> TelegramInteraction (Response Message)
sendVideo = ReaderT . const . Telegram.Bot.API.sendVideo

toSendVideoHere :: InputFile -> TelegramInteraction SendVideoRequest
toSendVideoHere pf = getChatId >>= \cid -> ReaderT $ const $ pure $ SendVideoRequest (SomeChatId cid) pf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

sendVideoHere :: InputFile -> TelegramInteraction (Response Message)
sendVideoHere t = toSendVideoHere t >>= Telegram.Bot.Monadic.sendVideo

sendAnimation :: SendAnimationRequest -> TelegramInteraction (Response Message)
sendAnimation = ReaderT . const . Telegram.Bot.API.sendAnimation

toSendAnimationHere :: InputFile -> TelegramInteraction SendAnimationRequest
toSendAnimationHere pf = getChatId >>= \cid -> ReaderT $ const $ pure $ SendAnimationRequest (SomeChatId cid) pf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

sendAnimationHere :: InputFile -> TelegramInteraction (Response Message)
sendAnimationHere t = toSendAnimationHere t >>= Telegram.Bot.Monadic.sendAnimation

sendVoice :: SendVoiceRequest -> TelegramInteraction (Response Message)
sendVoice = ReaderT . const . Telegram.Bot.API.sendVoice

toSendVoiceHere :: InputFile -> TelegramInteraction SendVoiceRequest
toSendVoiceHere pf = getChatId >>= \cid -> ReaderT $ const $ pure $ SendVoiceRequest (SomeChatId cid) pf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

sendVoiceHere :: InputFile -> TelegramInteraction (Response Message)
sendVoiceHere t = toSendVoiceHere t >>= Telegram.Bot.Monadic.sendVoice

sendVideoNote :: SendVideoNoteRequest -> TelegramInteraction (Response Message)
sendVideoNote = ReaderT . const . Telegram.Bot.API.sendVideoNote

toSendVideoNoteHere :: InputFile -> TelegramInteraction SendVideoNoteRequest
toSendVideoNoteHere pf = getChatId >>= \cid -> ReaderT $ const $ pure $ SendVideoNoteRequest (SomeChatId cid) pf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

sendVideoNoteHere :: InputFile -> TelegramInteraction (Response Message)
sendVideoNoteHere t = toSendVideoNoteHere t >>= Telegram.Bot.Monadic.sendVideoNote

sendMediaGroup :: SendMediaGroupRequest -> TelegramInteraction (Response [Message])
sendMediaGroup = ReaderT . const . Telegram.Bot.API.sendMediaGroup

toSendMediaGroupHere :: [InputMedia] -> TelegramInteraction SendMediaGroupRequest
toSendMediaGroupHere pf = getChatId >>= \cid -> ReaderT $ const $ pure $ SendMediaGroupRequest (SomeChatId cid) pf Nothing Nothing Nothing Nothing Nothing

sendMediaGroupHere :: [InputMedia] -> TelegramInteraction (Response [Message])
sendMediaGroupHere t = toSendMediaGroupHere t >>= Telegram.Bot.Monadic.sendMediaGroup

answerCallbackQuery :: AnswerCallbackQueryRequest -> TelegramInteraction (Response Bool)
answerCallbackQuery = ReaderT . const . Telegram.Bot.API.answerCallbackQuery

justAnswerCallbackQuery :: CallbackQueryId -> TelegramInteraction (Response Bool)
justAnswerCallbackQuery q = Telegram.Bot.Monadic.answerCallbackQuery (AnswerCallbackQueryRequest q Nothing Nothing Nothing Nothing)

editMessageText :: EditMessageTextRequest -> TelegramInteraction (Response EditMessageResponse)
editMessageText = ReaderT . const . Telegram.Bot.API.editMessageText

toEditMessageTextHere :: MessageId -> Text -> TelegramInteraction EditMessageTextRequest
toEditMessageTextHere m t = getChatId >>= \cid -> ReaderT $ const $ pure $ EditMessageTextRequest (Just $ SomeChatId cid) (Just m) Nothing t Nothing Nothing Nothing Nothing

editMessageTextHere :: MessageId -> Text -> TelegramInteraction (Response EditMessageResponse)
editMessageTextHere m t = toEditMessageTextHere m t >>= Telegram.Bot.Monadic.editMessageText

editMessageCaption :: EditMessageCaptionRequest -> TelegramInteraction (Response EditMessageResponse)
editMessageCaption = ReaderT . const . Telegram.Bot.API.editMessageCaption

toEditMessageCaptionHere :: MessageId -> Maybe Text -> TelegramInteraction EditMessageCaptionRequest
toEditMessageCaptionHere m t = getChatId >>= \cid -> ReaderT $ const $ pure $ EditMessageCaptionRequest (Just $ SomeChatId cid) (Just m) Nothing t Nothing Nothing Nothing

editMessageCaptionHere :: MessageId -> Maybe Text -> TelegramInteraction (Response EditMessageResponse)
editMessageCaptionHere m t = toEditMessageCaptionHere m t >>= Telegram.Bot.Monadic.editMessageCaption

editMessageMedia :: EditMessageMediaRequest -> TelegramInteraction (Response EditMessageResponse)
editMessageMedia = ReaderT . const . Telegram.Bot.API.editMessageMedia

toEditMessageMediaHere :: MessageId -> InputMedia -> TelegramInteraction EditMessageMediaRequest
toEditMessageMediaHere m t = getChatId >>= \cid -> ReaderT $ const $ pure $ EditMessageMediaRequest (Just $ SomeChatId cid) (Just m) Nothing t Nothing

editMessageMediaHere :: MessageId -> InputMedia -> TelegramInteraction (Response EditMessageResponse)
editMessageMediaHere m t = toEditMessageMediaHere m t >>= Telegram.Bot.Monadic.editMessageMedia

editMessageReplyMarkup :: EditMessageReplyMarkupRequest -> TelegramInteraction (Response EditMessageResponse)
editMessageReplyMarkup = ReaderT . const . Telegram.Bot.API.editMessageReplyMarkup

toEditMessageReplyMarkupHere :: MessageId -> Maybe SomeReplyMarkup -> TelegramInteraction EditMessageReplyMarkupRequest
toEditMessageReplyMarkupHere m t = getChatId >>= \cid -> ReaderT $ const $ pure $ EditMessageReplyMarkupRequest (Just $ SomeChatId cid) (Just m) Nothing t

editMessageReplyMarkupHere :: MessageId -> Maybe SomeReplyMarkup -> TelegramInteraction (Response EditMessageResponse)
editMessageReplyMarkupHere m t = toEditMessageReplyMarkupHere m t >>= Telegram.Bot.Monadic.editMessageReplyMarkup
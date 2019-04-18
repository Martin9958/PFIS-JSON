{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Mazo where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

--Afrom From Entity Mazo
mazoForm :: Maybe Mazo -> AForm Handler Mazo
mazoForm   mazo = Mazo
                 <$> areq textField "Nombre del Mazo " (mazoNombremazo <$> mazo)
                 <*> areq cantidadField "Cantidad de Cartas " (mazoCantidad <$> mazo)
               where
                 errorMessage :: Text
                 errorMessage = "No se puede realizar la accion"
                 cantidadField = checkBool (>0) errorMessage intField


--CRUD
--Create
getMazoNewR :: Handler Html
getMazoNewR = do
            (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ mazoForm Nothing
            defaultLayout $ do
                let actionR = MazoNewR
                $(widgetFile "Mazo")

postMazoNewR :: Handler Html
postMazoNewR = do
		((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ mazoForm  Nothing
		case result of
		     FormSuccess mazo -> do
				 _ <- runDB $ insertEntity mazo
				 redirect MazoListR
		     _ -> defaultLayout $ do
			let actionR = MazoNewR
			$(widgetFile "Mazo")

--Delete
postMazoDeleteR :: MazoId -> Handler ()
postMazoDeleteR mazoId = do
                    runDB $ delete mazoId
                    redirect MazoListR

--list
getMazoListR ::  Handler Html
getMazoListR  = do
                mazos <- runDB $ getAllMazos
                ( _ , _ ) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ mazoForm Nothing
                defaultLayout $ do
                   $(widgetFile "MazoView")


getAllMazos :: DB [Entity Mazo]
getAllMazos = selectList [] [Asc MazoNombremazo]


--Edit

getMazoEditR :: MazoId -> Handler Html
getMazoEditR mazoId  = do
               mazo <- runDB $ get404 mazoId
               (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ mazoForm  (Just mazo)
               defaultLayout $ do
                   let actionR = MazoEditR mazoId
                   $(widgetFile "Mazo")

postMazoEditR :: MazoId -> Handler Html
postMazoEditR mazoId  = do
                mazo <- runDB $ get404 mazoId
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ mazoForm  (Just mazo)
                case result of
                     FormSuccess mazoResult -> do
                                 _ <- runDB $ replace mazoId  mazoResult
                                 redirect MazoListR
                     _ -> defaultLayout $ do
                        let actionR = MazoEditR mazoId
                        $(widgetFile "Mazo")

--Search

getMazoSearchR ::  Handler Html
getMazoSearchR = do
           (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ mazoForm Nothing
           defaultLayout $ do
                let actionR = MazoSearchR
                $(widgetFile "MazoSearch")

postMazoSearchR :: Handler Html
postMazoSearchR = do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ mazoForm  Nothing
                case result of
                     FormSuccess _ -> do
                                 mazos <- runDB $ selectList [] []
                                 defaultLayout $ do
                                    $(widgetFile "MazoView")
                     _ -> defaultLayout $ do
                        let actionR = MazoSearchR
                        $(widgetFile "Mazo")


--JSON CRUD

getMazosJsonR :: Handler Value
getMazosJsonR = do
    mazos <- runDB $ selectList [] [] :: Handler [Entity Mazo]

    return $ object ["mazos" .= mazos]

postMazosJsonR :: Handler Value
postMazosJsonR = do
    mazo <- requireJsonBody :: Handler Mazo
    _    <- runDB $ insert mazo

    sendResponseStatus status201 ("CREATED" :: Text)

getMazoJsonR :: MazoId -> Handler Value
getMazoJsonR mazoId = do
    mazo <- runDB $ get404 mazoId

    return $ object ["mazos" .= (Entity mazoId mazo)]

putMazoJsonR :: MazoId -> Handler Value
putMazoJsonR mazoId = do
    mazo <- requireJsonBody :: Handler Mazo

    runDB $ replace mazoId mazo

    sendResponseStatus status200 ("UPDATED" :: Text)

deleteMazoJsonR :: MazoId -> Handler Value
deleteMazoJsonR mazoId = do
    runDB $ delete mazoId

    sendResponseStatus status200 ("DELETED" :: Text)
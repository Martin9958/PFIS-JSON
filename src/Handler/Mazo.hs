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
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- { "id": 1, "nombrecarta": "Dragón Blanco de Ojos Azules", "tipo": "Monstruo - Normal", "atributo": "Luz", "nivel":8, "descripcion": "Este legendario dragón es una poderosa máquina de destrucción. Virtualmente invencible, muy pocos se han enfrentado a esta impresionante criatura y han vivido para contarlo.", "ataque": 3000, "defensa": 2500, "icono": "軽い"}
instance ToJSON (Entity Carta) where
    toJSON (Entity cartaId carta) = object
        [ "id"         .= (String $ toPathPiece cartaId)
        , "nombrecarta"   .= cartaNombrecarta carta
        , "tipo"   .= cartaTipo carta
        , "atributo" .= cartaAtributo carta
        , "nivel" .= cartaNivel carta
        , "descripcion" .= cartaDescripcion carta
        , "ataque" .= cartaAtaque carta
        , "defensa" .= cartaDefensa carta
        , "icono" .= cartaIcono carta
        ]

instance FromJSON Carta where
        parseJSON (Object carta) = Carta
            <$> carta .: "nombrecarta"
            <*> carta .: "tipo"
            <*> carta .: "atributo"
            <*> carta .: "nivel"
            <*> carta .: "descripcion"
            <*> carta .: "ataque"
            <*> carta .: "defensa"
            <*> carta .: "icono"
        parseJSON _ = mzero

-- { "id": 1, "nombremazo": "Mazo de Kaiba", "cantidad": 40}
instance ToJSON (Entity Mazo) where
    toJSON (Entity mazoId mazo) = object
        [ "id"         .= (String $ toPathPiece mazoId)
        , "nombremazo"   .= mazoNombremazo mazo
        , "cantidad"   .= mazoCantidad mazo
        ]

instance FromJSON Mazo where
        parseJSON (Object mazo) = Mazo
            <$> mazo .: "nombremazo"
            <*> mazo .: "cantidad"
        parseJSON _ = mzero
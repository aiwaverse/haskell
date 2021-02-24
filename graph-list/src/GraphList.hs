{- |
Copyright: (c) 2020 Aiwa
SPDX-License-Identifier: MIT
Maintainer: Aiwa <aiwavision@protonmail.com>

See README for more info
-}
{-# LANGUAGE OverloadedStrings #-}
module GraphList
  ( mapa
  , estaNaLista
  , subtraiLista
  , vizinhos
  , encontraCaminho
  , encontraCaminhoVizinhos
  , montaCaminhosMaybe
  , encontraTodosCaminhos
  , encontraTodosCaminhosVizinhos
  ) where

import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , isNothing
                                                )
import qualified Data.Text                     as T

data Node = MkNode
  { nodeNome     :: T.Text
  , nodeVizinhos :: [T.Text]
  }

type Grafo = [Node]

helena :: Node
helena = MkNode "Helena" ["Winnipeg", "Duluth", "Omaha", "Denver"]
denver :: Node
denver = MkNode "Denver" ["Helena", "Omaha", "Kansas City", "Oklahoma City", "Santa Fe"]
santaFe :: Node
santaFe = MkNode "Santa Fe" ["Denver", "Oklahoma City"]
winnipeg :: Node
winnipeg = MkNode "Winnipeg" ["Sault St. Marie", "Duluth", "Helena"]
duluth :: Node
duluth =
  MkNode "Duluth" ["Winnipeg", "Sault St. Marie", "Toronto", "Chicago", "Omaha", "Helena"]
omaha :: Node
omaha = MkNode "Omaha" ["Kansas City", "Denver", "Helena", "Duluth", "Chicago"]
kansasCity :: Node
kansasCity = MkNode "Kansas City" ["Omaha", "Saint Louis", "Oklahoma City", "Denver"]
oklahomaCity :: Node
oklahomaCity =
  MkNode "Oklahoma City" ["Denver", "Kansas City", "Little Rock", "Santa Fe"]
saultStMarie :: Node
saultStMarie = MkNode "Sault St. Marie" ["Winnipeg", "Toronto", "Duluth"]
chicago :: Node
chicago = MkNode "Chicago" ["Toronto", "Pittsburgh", "Saint Louis", "Omaha", "Duluth"]
saintLouis :: Node
saintLouis = MkNode
  "Saint Louis"
  ["Chicago", "Pittsburgh", "Nashville", "Little Rock", "Kansas City"]
littleRock :: Node
littleRock = MkNode "Little Rock" ["Saint Louis", "Nashville", "Oklahoma City"]
toronto :: Node
toronto = MkNode "Toronto" ["Sault St. Marie", "Duluth", "Chicago", "Pittsburgh"]
pittsburgh :: Node
pittsburgh = MkNode "Pittsburgh" ["Toronto", "Chicago", "Saint Louis", "Nashville"]
nashville :: Node
nashville = MkNode "Nashville" ["Pittsburgh", "Saint Louis", "Little Rock", "Atlanta"]
atlanta :: Node
atlanta = MkNode "Atlanta" ["Nashville"]

mapa :: Grafo
mapa =
  [ helena
  , denver
  , santaFe
  , winnipeg
  , duluth
  , omaha
  , kansasCity
  , oklahomaCity
  , saultStMarie
  , chicago
  , saintLouis
  , littleRock
  , nashville
  , pittsburgh
  , atlanta
  , toronto
  ]

estaNaLista :: T.Text -> [T.Text] -> Bool
estaNaLista = elem

subtraiLista :: [T.Text] -> [T.Text] -> [T.Text]
subtraiLista lista1 lista2 =
  foldr (\elm acc -> if elm `estaNaLista` lista2 then acc else elm : acc) [] lista1

-- | a entrada deve ser o nome de um nodo, um grafo e uma
-- lista de nomes de nodos (já visitados), nesta ordem, e a saída deve ser
-- a lista dos nomes dos vizinhos deste nodo que não constam da lista de
-- (nomes de nodos de) entrada
vizinhos :: T.Text -> Grafo -> [T.Text] -> [T.Text]
vizinhos _    []       _              = error "Grafo não possuí o nodo!"
vizinhos nodo (x : xs) nodosVisitados = if nodo == nodeNome x
  then nodeVizinhos x `subtraiLista` nodosVisitados
  else vizinhos nodo xs nodosVisitados

encontraCaminho :: T.Text -> T.Text -> Grafo -> [T.Text] -> Maybe [T.Text]
encontraCaminho origem destino grafo visitadas
  | origem == destino         = Just [destino]
  | isNothing caminhoPossivel = Nothing
  | otherwise                 = Just $ origem : fromJust caminhoPossivel
 where
  caminhoPossivel = encontraCaminhoVizinhos (vizinhos origem grafo visitadasNovo)
                                            destino
                                            grafo
                                            visitadasNovo
  visitadasNovo = visitadas ++ [origem]

encontraCaminhoVizinhos :: [T.Text] -> T.Text -> Grafo -> [T.Text] -> Maybe [T.Text]
encontraCaminhoVizinhos [] _ _ _ = Nothing
encontraCaminhoVizinhos (x : xs) destino grafo visitadas =
  let caminhoPossivel = encontraCaminho x destino grafo visitadas
  in  case caminhoPossivel of
        Nothing -> encontraCaminhoVizinhos xs destino grafo visitadas
        _       -> caminhoPossivel

montaCaminhosMaybe :: T.Text -> [Maybe [T.Text]] -> [Maybe [T.Text]]
montaCaminhosMaybe _    []            = []
montaCaminhosMaybe nome (Just x : xs) = Just (nome : x) : montaCaminhosMaybe nome xs

encontraTodosCaminhos :: T.Text -> T.Text -> Grafo -> [T.Text] -> [Maybe [T.Text]]
encontraTodosCaminhos origem destino grafo visitadas
  | origem == destino = [Just [destino]]
  | otherwise         = montaCaminhosMaybe origem caminhosReais
 where
  caminhosPossiveis = encontraTodosCaminhosVizinhos
    (vizinhos origem grafo visitadasNovo)
    destino
    grafo
    visitadasNovo
  visitadasNovo = visitadas ++ [origem]
  caminhosReais = filter isJust caminhosPossiveis

encontraTodosCaminhosVizinhos
  :: [T.Text] -> T.Text -> Grafo -> [T.Text] -> [Maybe [T.Text]]
encontraTodosCaminhosVizinhos [] _ _ _ = [Nothing]
encontraTodosCaminhosVizinhos (x : xs) destino grafo visitadas
  | any isNothing caminhoPossivel = filter isJust caminhoPossivel
  | otherwise =  caminhoPossivel
  ++ encontraTodosCaminhosVizinhos xs destino grafo visitadas
  where caminhoPossivel = encontraTodosCaminhos x destino grafo visitadas

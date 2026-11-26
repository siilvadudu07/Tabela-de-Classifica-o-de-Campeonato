-- PjBL2: Tabela de Classificacao de Campeonato (ambiente: play.haskell.org)
module Main (main) where

import Data.Char (isSpace)
import Data.Either (lefts, rights)
import Data.List (intercalate, maximumBy, nub, sort, sortBy)
import Data.Ord (Down (..), comparing)
import Text.Read (readMaybe)

-- ============================================================
-- CAMADA 1: Tipos e dados do campeonato
-- ============================================================

-- Camada 1: tipos e consultas basicas do campeonato.


type Time = String

data Resultado = Vitoria | Empate | Derrota
  deriving (Show, Eq)

data Partida = Partida
  { rodada        :: Int
  , mandante      :: Time
  , golsMandante  :: Int
  , golsVisitante :: Int
  , visitante     :: Time
  } deriving (Show, Eq)

data Linha = Linha
  { time       :: Time
  , jogos      :: Int
  , vitorias   :: Int
  , empates    :: Int
  , derrotas   :: Int
  , golsPro    :: Int
  , golsContra :: Int
  } deriving (Show, Eq)

participou :: Time -> Partida -> Bool
participou t p = t == mandante p || t == visitante p

resultadoPara :: Time -> Partida -> Maybe Resultado
resultadoPara t p
  | not (participou t p) = Nothing
  | t == mandante p      = Just (comparaGols (golsMandante p) (golsVisitante p))
  | otherwise            = Just (comparaGols (golsVisitante p) (golsMandante p))
  where
    comparaGols pro contra
      | pro > contra  = Vitoria
      | pro == contra = Empate
      | otherwise     = Derrota

times :: [Partida] -> [Time]
times ps = sort (nub (concatMap (\p -> [mandante p, visitante p]) ps))

-- ============================================================
-- CAMADA 2: Classificacao com funcoes de ordem superior
-- ============================================================

-- Camada 2: classificacao com funcoes de ordem superior (sem recursao explicita).


pontos :: Resultado -> Int
pontos Vitoria = 3
pontos Empate  = 1
pontos Derrota = 0

linhaVazia :: Time -> Linha
linhaVazia t = Linha t 0 0 0 0 0 0

-- Passo da dobra: incorpora uma partida a linha do time (ignora partidas em que ele nao jogou).
atualizaLinha :: Linha -> Partida -> Linha
atualizaLinha l p = case resultadoPara (time l) p of
    Nothing  -> l
    Just res -> l
        { jogos      = jogos l + 1
        , vitorias   = vitorias l + fromEnum (res == Vitoria)
        , empates    = empates l + fromEnum (res == Empate)
        , derrotas   = derrotas l + fromEnum (res == Derrota)
        , golsPro    = golsPro l + golsFeitos
        , golsContra = golsContra l + golsTomados
        }
  where
    (golsFeitos, golsTomados)
      | time l == mandante p = (golsMandante p, golsVisitante p)
      | otherwise            = (golsVisitante p, golsMandante p)

linhaDoTime :: [Partida] -> Time -> Linha
linhaDoTime ps t = foldl atualizaLinha (linhaVazia t) ps

pontosLinha :: Linha -> Int
pontosLinha l =
    vitorias l * pontos Vitoria + empates l * pontos Empate + derrotas l * pontos Derrota

saldo :: Linha -> Int
saldo l = golsPro l - golsContra l

aproveitamento :: Linha -> Double
aproveitamento l
    | jogos l == 0 = 0
    | otherwise    = 100 * fromIntegral (pontosLinha l) / fromIntegral (jogos l * pontos Vitoria)

-- Criterios: pontos, vitorias, saldo de gols, gols pro e, por fim, ordem alfabetica
-- (so para a ordem ser deterministica).
classificacao :: [Partida] -> [Linha]
classificacao ps = sortBy ordenacao (map (linhaDoTime ps) (times ps))
  where
    ordenacao = comparing (Down . pontosLinha)
             <> comparing (Down . vitorias)
             <> comparing (Down . saldo)
             <> comparing (Down . golsPro)
             <> comparing time

ateRodada :: Int -> [Partida] -> [Partida]
ateRodada r = filter ((<= r) . rodada)

-- Em caso de empate na diferenca, maximumBy devolve a ultima partida da lista.
maiorGoleada :: [Partida] -> Maybe Partida
maiorGoleada ps
    | null ps   = Nothing
    | otherwise = Just (maximumBy (comparing diferenca) ps)
  where
    diferenca p = abs (golsMandante p - golsVisitante p)

mediaGols :: [Partida] -> Maybe Double
mediaGols ps
    | null ps   = Nothing
    | otherwise = Just (fromIntegral (sum (map totalGols ps)) / fromIntegral (length ps))
  where
    totalGols p = golsMandante p + golsVisitante p

-- ============================================================
-- CAMADA 3: Leitura, validacao e saida
-- ============================================================

-- Camada 3: leitura, validacao (Maybe/Either) e formatacao da saida.


data Erro
  = CamposInvalidos Int          -- numero da linha
  | NumeroInvalido Int           -- numero da linha
  | PlacarNegativo Int           -- numero da linha
  | MesmoTime Int                -- numero da linha
  | TimeRepetidoNaRodada Int     -- numero da RODADA
  deriving (Show, Eq)

descreverErro :: Erro -> String
descreverErro (CamposInvalidos n) =
    "Linha " ++ show n ++ ": esperados 5 campos (rodada;mandante;gols mandante;gols visitante;visitante), com nomes de time nao vazios"
descreverErro (NumeroInvalido n) =
    "Linha " ++ show n ++ ": rodada ou placar nao e um numero inteiro"
descreverErro (PlacarNegativo n) =
    "Linha " ++ show n ++ ": o placar nao pode ser negativo"
descreverErro (MesmoTime n) =
    "Linha " ++ show n ++ ": um time nao pode jogar contra si mesmo"
descreverErro (TimeRepetidoNaRodada r) =
    "Rodada " ++ show r ++ ": ha time escalado mais de uma vez"

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

separar :: Char -> String -> [String]
separar delim = foldr passo [[]]
  where
    passo c acc
      | c == delim = [] : acc
      | otherwise  = case acc of
                       (x : xs) -> (c : x) : xs
                       []       -> [[c]]

exigir :: Bool -> Erro -> Either Erro ()
exigir ok erro = if ok then Right () else Left erro

cincoCampos :: Int -> [String] -> Either Erro (String, String, String, String, String)
cincoCampos _ [a, b, c, d, e] = Right (a, b, c, d, e)
cincoCampos n _               = Left (CamposInvalidos n)

numero :: Int -> String -> Either Erro Int
numero n = maybe (Left (NumeroInvalido n)) Right . readMaybe

lerPartida :: Int -> String -> Either Erro Partida
lerPartida n linha = do
    (sr, m, sgm, sgv, v) <- cincoCampos n (map trim (separar ';' linha))
    exigir (not (null m) && not (null v)) (CamposInvalidos n)
    r  <- numero n sr
    gm <- numero n sgm
    gv <- numero n sgv
    exigir (gm >= 0 && gv >= 0) (PlacarNegativo n)
    exigir (m /= v) (MesmoTime n)
    return (Partida r m gm gv v)

-- Linhas de dados (com o numero original), sem brancos e sem comentarios (#).
linhasUteis :: String -> [(Int, String)]
linhasUteis = filter (ehDado . snd) . zip [1 ..] . lines
  where
    ehDado l = case trim l of
        ""        -> False
        ('#' : _) -> False
        _         -> True

encontraRepetidos :: [Partida] -> [Erro]
encontraRepetidos ps = [TimeRepetidoNaRodada r | r <- nub (map rodada ps), temRepetido r]
  where
    temRepetido r =
        let escalados = concatMap (\p -> [mandante p, visitante p]) (filter ((== r) . rodada) ps)
        in length escalados /= length (nub escalados)

verificaRepetidos :: [Partida] -> Either Erro [Partida]
verificaRepetidos ps = case encontraRepetidos ps of
    []      -> Right ps
    (e : _) -> Left e

lerCampeonato :: String -> Either Erro [Partida]
lerCampeonato texto = do
    partidas <- traverse (uncurry lerPartida) (linhasUteis texto)
    verificaRepetidos partidas

todosOsErros :: String -> [Erro]
todosOsErros texto = lefts resultados ++ encontraRepetidos (rights resultados)
  where
    resultados = map (uncurry lerPartida) (linhasUteis texto)

tabelaFormatada :: [Linha] -> String
tabelaFormatada linhas = unlines (cabecalho : zipWith formata [1 :: Int ..] linhas)
  where
    cabecalho = celulas ["Pos", "Time", "Pts", "J", "V", "E", "D", "SG", "Aprov(%)"]
    formata pos l = celulas
        [ show pos, time l, show (pontosLinha l), show (jogos l), show (vitorias l)
        , show (empates l), show (derrotas l), show (saldo l), show (arredonda (aproveitamento l)) ]
    celulas = intercalate " | " . zipWith preencher larguras
    larguras :: [Int]
    larguras = [3, 12, 3, 2, 2, 2, 2, 3, 9]
    preencher n s = s ++ replicate (n - length s) ' '
    arredonda :: Double -> Double
    arredonda d = fromIntegral (round (d * 100) :: Int) / 100

-- ============================================================
-- DADOS: campeonato de teste (como String)
-- ============================================================

-- Dados do campeonato de teste (como String, para funcionar sem entrada de teclado).

-- 6 times, 5 rodadas. Tem 0 x 0 (Corinthians x Sao Paulo), goleada de 3 gols
-- (Palmeiras 3 x 0 Flamengo) e tres times empatados com 9 pontos no final.
campeonatoValido :: String
campeonatoValido = unlines
  [ "# rodada; mandante; gols mandante; gols visitante; visitante"
  , "1; Palmeiras; 3; 0; Flamengo"
  , "1; Corinthians; 0; 0; Sao Paulo"
  , "1; Fluminense; 1; 0; Gremio"
  , "2; Flamengo; 1; 0; Corinthians"
  , "2; Sao Paulo; 1; 0; Fluminense"
  , "2; Gremio; 1; 0; Palmeiras"
  , "3; Palmeiras; 1; 0; Corinthians"
  , "3; Flamengo; 1; 0; Fluminense"
  , "3; Sao Paulo; 1; 0; Gremio"
  , "4; Fluminense; 1; 0; Palmeiras"
  , "4; Gremio; 1; 0; Corinthians"
  , "4; Flamengo; 1; 0; Sao Paulo"
  , "5; Palmeiras; 1; 0; Sao Paulo"
  , "5; Gremio; 1; 0; Flamengo"
  , "5; Corinthians; 1; 0; Fluminense"
  ]

-- Um erro de cada tipo (linhas 2 a 5) e um time repetido na rodada 2 (linhas 6 e 7).
campeonatoInvalido :: String
campeonatoInvalido = unlines
  [ "# rodada; mandante; gols mandante; gols visitante; visitante"
  , "1; Palmeiras; tres; 0; Flamengo"
  , "1; Corinthians; -1; 0; Sao Paulo"
  , "1; Fluminense; 1; 0; Fluminense"
  , "1; Gremio; 1"
  , "2; Gremio; 1; 0; Palmeiras"
  , "2; Gremio; 1; 0; Corinthians"
  ]

-- ============================================================
-- MAIN: demonstracao das tres camadas
-- ============================================================

-- Demonstracao das tres camadas. Unica parte com IO: o main.


secao :: String -> String
secao titulo = "\n>>> " ++ titulo ++ " <<<\n"

descreverPartida :: Partida -> String
descreverPartida p =
    "Rodada " ++ show (rodada p) ++ ": " ++ mandante p ++ " " ++ show (golsMandante p)
    ++ " x " ++ show (golsVisitante p) ++ " " ++ visitante p

relatorioValido :: String -> String
relatorioValido texto = case lerCampeonato texto of
    Left erro -> secao "ERRO AO CARREGAR O CAMPEONATO" ++ descreverErro erro ++ "\n"
    Right ps -> concat
        [ secao "CLASSIFICACAO FINAL"
        , tabelaFormatada (classificacao ps)
        , secao "CLASSIFICACAO APOS A RODADA 3"
        , tabelaFormatada (classificacao (ateRodada 3 ps))
        , secao "ESTATISTICAS"
        , "Maior goleada: " ++ maybe "nenhuma partida" descreverPartida (maiorGoleada ps) ++ "\n"
        , "Media de gols por partida: " ++ maybe "sem partidas" show (mediaGols ps) ++ "\n"
        ]

relatorioErros :: String -> String
relatorioErros texto = concat
    [ secao "ENTRADA COM ERROS"
    , "Primeiro erro (lerCampeonato): "
        ++ either descreverErro (const "nenhum") (lerCampeonato texto) ++ "\n"
    , "Todos os erros (todosOsErros):\n"
    , unlines (map (("  - " ++) . descreverErro) (todosOsErros texto))
    ]

relatorioVazio :: String
relatorioVazio = concat
    [ secao "CAMPEONATO VAZIO"
    , either descreverErro (\ps -> tabelaFormatada (classificacao ps)) (lerCampeonato "")
    , "Maior goleada: " ++ maybe "nenhuma partida" descreverPartida (maiorGoleada []) ++ "\n"
    ]

main :: IO ()
main = putStr (relatorioValido campeonatoValido ++ relatorioErros campeonatoInvalido ++ relatorioVazio)

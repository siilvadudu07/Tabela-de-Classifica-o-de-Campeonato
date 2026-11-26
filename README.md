# PjBL2 - Tabela de Classificacao de Campeonato (Haskell)

Ambiente utilizado: **playground online (https://play.haskell.org)**.
Versao: GHC 9.10.3 (otimizacao -O1), testado tambem com `{-# OPTIONS_GHC -Wall #-}`.
Apenas a biblioteca padrao (`base`) e usada.

Integrantes:
- José Eduardo da Silva
- Nicolas Oswaldo Letti
- Ricardo Magno Yomura

Professor: Felipe Augusto Przysiada

## Arquivos

- `Main.hs`: o programa completo, em secoes comentadas na ordem dos modulos
  (Camada 1 Tipos, Camada 2 Classificacao, Camada 3 Leitura, Dados, Main).
- `Testes.hs`: as mesmas camadas mais os testes com resultado esperado (o `main` dele roda os testes).
- `docs/decisoes.md`: decisoes de modelagem e limitacoes conhecidas.

## Como executar

1. Abra https://play.haskell.org
2. Apague o codigo de exemplo, cole o conteudo de `Main.hs` e clique em **Run**.
   A saida mostra a tabela final, a tabela apos a rodada 3, as estatisticas e o tratamento de erros.
3. Para os testes, faca o mesmo com o conteudo de `Testes.hs`. Cada teste imprime `OK nome` ou `FALHOU nome: valor obtido`.

Os dados do campeonato ficam no proprio codigo (secao DADOS), pois o playground nao tem entrada pelo teclado.

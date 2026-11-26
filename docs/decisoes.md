# Decisoes de modelagem e limitacoes conhecidas

## Decisoes

- **Partida e Linha como records; Resultado como tipo soma.** Nomes de campo evitam trocar gols do
  mandante com os do visitante; `Vitoria | Empate | Derrota` impede valores como "v" ou 7.
- **`resultadoPara` devolve `Maybe Resultado`.** Um time que nao jogou a partida e `Nothing`, e nao
  um resultado inventado. `atualizaLinha` usa isso para ignorar partidas alheias dentro da dobra.
- **`linhaDoTime` usa `foldl`.** O acumulador e uma `Linha` que recebe as partidas em ordem; a lista
  e pequena, entao a versao preguicosa nao traz problema (`foldl'` seria a opcao para listas grandes).
- **`pontosLinha` e `aproveitamento` usam `pontos`.** O valor 3/1/0 fica definido em um unico lugar.
- **Criterios de desempate (estilo Brasileirao, sem confronto direto):** pontos, numero de vitorias,
  saldo de gols, gols pro e, por ultimo, ordem alfabetica do nome (so para a ordem ser deterministica).
  Tudo em uma unica funcao de comparacao: `comparing (Down . ...) <> ...` (`Ordering` e um `Monoid`).
- **Funcoes parciais evitadas.** `maiorGoleada` e `mediaGols` devolvem `Maybe`; nao ha `head`,
  `!!` nem `read`. A leitura usa `readMaybe` e casamento de padrao em lista de 5 elementos.
- **`Either Erro` com notacao `do`.** `lerPartida` para no primeiro erro da linha; `lerCampeonato`
  aplica `traverse` as linhas e depois verifica time repetido na rodada. `todosOsErros` usa
  `lefts`/`rights` para acumular todos os erros.
- **Erros carregam o numero da linha do texto original** (contando comentarios e brancos), exceto
  `TimeRepetidoNaRodada`, que carrega a rodada. `descreverErro` gera a mensagem legivel.
- **Nucleo puro e `main` fino.** Todo o relatorio e montado por funcoes que devolvem `String`; o
  `main` so chama `putStr`. Os dados ficam na secao DADOS do `Main.hs`, sem `getLine`.
- **Campeonato vazio e valido:** `lerCampeonato ""` devolve `Right []`, a classificacao e `[]` e a
  tabela impressa tem so o cabecalho.

## Limitacoes conhecidas

- Nao ha desempate por confronto direto, cartoes ou sorteio.
- Em empate na diferenca de gols, `maiorGoleada` devolve a ultima partida da lista.
- Nomes de time diferem por maiusculas/acentos ("Gremio" e "gremio" sao times distintos).
- A rodada nao e validada (rodada 0 ou negativa e aceita) e o mesmo confronto pode aparecer em
  rodadas diferentes sem erro. Nao ha verificacao de calendario completo.
- `todosOsErros` so verifica time repetido na rodada entre as linhas que foram lidas sem erro.
- Os dados de teste nao usam acentos, para evitar problemas de codificacao no terminal.
- `aproveitamento` assume 3 pontos por vitoria (via `pontos Vitoria`).

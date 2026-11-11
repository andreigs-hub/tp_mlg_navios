# Trabalho: Sobreviventes do Titanic (MLG)

Análise do naufrágio do Titanic (14/04/1912) com Modelos Lineares Generalizados (regressão binária). Compara as ligações logit, probit e cloglog, com validação cruzada e avaliação em teste separado.

## Arquivos

- `titanic.csv`: banco de dados com 714 passageiros e as variáveis `vivo`, `classe`, `idade`, `sexo` e `sozinho`.
- `trabalho-titanic.qmd`: código e texto do trabalho, em Quarto.
- `trabalho-titanic.pdf`: versão em PDF gerada a partir do `.qmd`.

## Como reproduzir

1. Instale o R, o Quarto e um LaTeX (por exemplo, TinyTeX).
2. Instale os pacotes no R:

   ```r
   install.packages(c("tidyverse", "tidymodels", "pROC"))

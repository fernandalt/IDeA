# IDeA
O Indicador de Desigualdades e Aprendizagens (IDeA) sintetiza informações do nível de aprendizagem dos estudantes e da desigualdade entre grupos sociais específicos (raça, sexo e nível socioeconômico). As medidas foram calculadas para os municípios brasileiros, em quatro períodos distintos. Nesse sentido, é possível avaliar a evolução histórica do indicador.

### > Dados
O Indicador foi contruído a partir dos dados do Sistema de Avaliação da Educação Básica (SAEB) de 2007 a 2017. Nos códigos, a base que combina essas informações é importada da pasta raw como *DadosProvaBrasil_2007_2017.Rdata*. Os micro dados estão disponível no site do Instituto Nacional de Estudos e Pesquisas Educacionais Anísio Teixeira (INEP) https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/saeb.

Os dados do SAEB, assim como do Censo Escolar de 2017, disponíveis em https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar, foram usados para calcular a proporção de alunos por nível socioeconômico, raça e sexo em cada escola e município.

Para a definição das faixas do nível de aprendizagem foram utilizados os resultados do Índice de Desenvolvimento da Educação Básica (Ideb) de 2017, a nível municipal. Os micro dados estão disponível em https://www.gov.br/inep/pt-br/areas-de-atuacao/pesquisas-estatisticas-e-indicadores/ideb/resultados.

As medidas do nível de aprendizagem e desigualdades calculadas estão disponíveis na pasta de dados. Bem como resultados produzidos em etapas intermediária: proporção de estudantes em grupos sociais específicos por escola e município, efeitos dos modelos de regressão multinível, dados simulados, pontos de corte das faixas de classificação da KL.


### > Códigos
Os resultados do IDeA poderão ser reproduzidos a partir da execução dos códigos disponibilizados. Mais informações acerca da metodologia são fornecidas na documentação técnica do projeto. 

Em suma, após a obtenção das bases de dados citadas acima, os modelos devem ser ajustados e os efeitos estimados para o nivel socioeconômico 5 devem ser corrigidos. Na sequência, é necessário calcular a proporção de estudantes por grupos nas escolas e municípios e então realizar a simulação de estudantes para complementar as amostras. Por fim, basta calcular a KL e definir as faixas interpretativas para o nível de aprendizagem e desigualdades. 

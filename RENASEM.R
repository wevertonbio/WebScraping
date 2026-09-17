# Carregar pacotes
library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(data.table)
library(pbapply)


# Criar looping
estados <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE",
             "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR",
             "SC", "RS", "MS", "MT", "GO", "DF")

# Ver estados prontos
estados_prontos <- list.files(pattern = "renasem_")
estados_prontos <- gsub("renasem_", "", estados_prontos) |>
  fs::path_ext_remove()

estados <- setdiff(estados, estados_prontos)

lapply(estados, function(estado){
  # Defina o Estado
  #estado <- "PB"

  message("Obtendo informações de ", estado, "\n")

  #### 1. Iniciar Sessão ####
  b <- ChromoteSession$new()
  b$view()


  # Acessar site
  url_consulta <- "https://sistemasweb.agricultura.gov.br/renasem/psq_consultarenasems.do"
  b$Page$navigate(url_consulta)
  # Aumentar zoom, se quiser
  b$Runtime$evaluate("document.body.style.zoom = '1.5';")

  #### 3. Selecionar UF ####
  js_code <- sprintf('
  var selectUF = document.querySelector("select[name*=\'uf\'], select[id*=\'uf\'], select[name*=\'sgUf\'], select[name*=\'estado\']");
  if (selectUF) {
    selectUF.value = "%s";
    selectUF.dispatchEvent(new Event("change", { bubbles: true }));
  }
', estado)
  b$Runtime$evaluate(expression = js_code)

  # Clica diretamente no botão "Pesquisa" pelo ID 'botaoPesquisar'
  b$Runtime$evaluate(expression = "document.getElementById('botaoPesquisar').click();")
  # Aguarda carregar
  Sys.sleep(5)

  # Extrair total de páginas
  res_paginas <- b$Runtime$evaluate(expression = '
  (function() {
    // Procura qualquer elemento de texto que contenha "Página" ou "Páginas" seguido de "de"
    var todosElementos = Array.from(document.querySelectorAll("td, span, div, b"));
    var el = todosElementos.find(e => /Páginas?\\s+\\d+\\s+de\\s+\\d+/i.test(e.innerText));
    return el ? el.innerText.trim() : null;
  })()
')
  texto_paginacao <- res_paginas$result$value

  # 2. Extrai apenas o número total após o "de"
  total_paginas <- as.numeric(str_extract(texto_paginacao, "(?<=de\\s)\\d+"))

  message("Extraindo ", total_paginas, " páginas...")

  #### Looping para extrair informações ####
  res <- pblapply(1:total_paginas, function(i){
    try({
      #### Atualizar página, se necessario ####
      if(i > 1){
        #### Ir para proxima pagina
        # Clicar no botão 'Próximo' ou disparar a função JS diretamente
        b$Runtime$evaluate(expression = '
  (function() {
    var btn = document.querySelector("img[title=\'Próximo\'], img[src*=\'bt_seta_direita.gif\']");
    if (btn) {
      btn.click();
      return "Clique efetuado no botão Próximo.";
    } else if (typeof setvalor === "function" && typeof submete === "function") {
      setvalor("componenteSelecionado", "grid");
      submete("gridPesquisaProximo");
      return "Função JS gridPesquisaProximo executada.";
    }
    return "Elemento não encontrado.";
  })()
')
        # Aguardar a nova página carregar
        Sys.sleep(5)
      }

      #### 4. Extrair Tabela de Resultados ####
      res_js <- b$Runtime$evaluate(expression = '
  (function() {
    // Busca os cabeçalhos
    let headers = Array.from(document.querySelectorAll("th.labelCampo"))
                       .map(th => th.innerText.trim());

    // Se não encontrou cabeçalhos com essa classe, pega qualquer th da página
    if (headers.length === 0) {
      headers = Array.from(document.querySelectorAll("th"))
                     .map(th => th.innerText.trim());
    }

    // Localiza todas as linhas de dados (tr que contêm td)
    let rows = Array.from(document.querySelectorAll("tr"))
                    .filter(tr => tr.querySelectorAll("td").length >= headers.length && tr.querySelectorAll("td").length > 0);

    let data = rows.map(tr => {
      let cells = Array.from(tr.querySelectorAll("td")).map(td => td.innerText.trim());
      let rowObj = {};
      headers.forEach((h, idx) => {
        rowObj[h || ("col_" + idx)] = cells[idx] || "";
      });
      return rowObj;
    });

    return JSON.stringify(data);
  })()
')

      # 4.1. Converte o JSON retornado pelo Chrome em Data Frame no R
      json_texto <- res_js$result$value
      df_renasem <- jsonlite::fromJSON(json_texto)

      # 4.2. Limpeza e extração explícita de CNPJ/CPF
      df_final <- as.data.frame(df_renasem) %>%
        janitor::clean_names() %>%
        mutate(across(everything(), as.character)) %>%
        mutate(
          cnpj_cpf = str_extract(
            apply(., 1, paste, collapse = " "),
            "\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}|\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}"
          )
        )

      # Remover linhas
      df_final <- df_final |>
        filter(uf == estado)

      # 4.3. Ver as primeiras linhas
      # head(df_final)

      return(df_final)
    }) #End of try
  })

  res2 <- res[sapply(res, is.data.frame)]

  # Unir dados
  res_final <- rbindlist(res2)

  #Salvar
  fwrite(res_final,
         paste0("renasem_", estado, ".csv"))
})

# Merge data
l <- list.files(pattern = "renasem_", full.names = TRUE)
d <- pblapply(l, fread)
d <- rbindlist(d)

# Ver estados com mais registros
table(d$uf) |> sort()

# Salvar
fwrite(d, "dados_renasem.csv")

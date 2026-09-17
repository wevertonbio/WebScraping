#### Obter dados do WebAmbiente ####
# https://www.webambiente.cnptia.embrapa.br/publico/especies.xhtml

# Autor: Weverton C. F. Trindade
# E-mail: wevertonf1993@gmail.com

# Script inspirado em:
# https://github.com/pedrosiracusa/lando/blob/master/data_gathering%2Fscraping_webambiente.ipynb

# Carregar pacotes
library(chromote)
library(dplyr)
library(stringr)

#### Helpers ####
botao_id <- "j_idt78:selectEspecieForm:j_idt90"
# script_clique <- sprintf("document.getElementById('%s').click()", botao_id)
script_abrir_especie <- "document.querySelectorAll('tr.ui-widget-content')[0].click()"
extrair_especie_com_na <- function(html) {

  # Seleciona todas as linhas de informação (as que contêm rótulos e valores)
  linhas <- html %>% html_nodes("tr.pg_infogeral_row1, tr.pg_infogeral_row2")

  labels_finais <- c()
  valores_finais <- c()

  for (linha in linhas) {
    # Tenta extrair o rótulo
    label_node <- linha %>% html_node(".pg_infogeral_label")
    label_txt <- if (!is.na(label_node)) html_text(label_node, trim = TRUE) else NA

    # Tenta extrair o valor
    valor_node <- linha %>% html_node(".pg_infogeral_valor")
    valor_txt <- if (!is.na(valor_node)) html_text2(valor_node) else NA

    # Limpeza do rótulo (remover os ":")
    if (!is.na(label_txt) && label_txt != "") {
      label_limpo <- gsub(":", "", label_txt)

      # Tratamento de valor vazio como NA
      # Se o texto for apenas espaços ou vazio, vira NA
      if (is.na(valor_txt) || nchar(trimws(valor_txt)) == 0) {
        valor_txt <- NA
      }

      labels_finais <- c(labels_finais, label_limpo)
      valores_finais <- c(valores_finais, valor_txt)
    }
  }

  # Se não encontrou nenhuma linha, retorna um DF vazio
  if (length(labels_finais) == 0) return(data.frame())

  # Transforma em data frame de uma linha
  df <- as.data.frame(t(valores_finais), stringsAsFactors = FALSE)
  colnames(df) <- labels_finais

  return(df)
}
extrair_campo_seguro <- function(texto, campo) {
  padrao <- sprintf("(?:^|\\n)%s:\\s*([^\\n:]+)", campo)

  match <- str_match(texto, padrao)

  if (!is.na(match[1, 2])) {
    valor <- str_trim(match[1, 2])
    # Limpeza adicional para remover tabs residuais
    valor <- str_replace_all(valor, "\\t", " ")
    if(valor %in% campos_alvo | valor == "[Saiba mais]"){
      valor <- NA}
    return(valor)
  } else {
    return(NA) # Retorna NA se o campo não existir ou estiver vazio
  }
}
campos_alvo <- c(
  "Nome Popular", "Sinonímia", "Família", "Bioma",
  "Formação Vegetal", "Fitofisionomias", "Presença nos estados",
  "Risco de extinção", "Período de coleta de sementes", "Beneficiamento de sementes",
  "Porcentagem de germinação", "Substrato da muda",
  "Desenvolvimento da muda no viveiro",
  "Número de sementes/kg", "Armazenamento", "Semeadura",
  "Nível de sombreamento da muda no viveiro",
  "Tolerância a Sombra", "Estratégia ecológica de ocupação",
  "Desenvolvimento da muda no campo", "Porte da planta",
  "Período de floração", "Período de frutificação",
  "Uso Econômico", "Polinização", "Dispersão"
)
gerar_sequencia_clique <- function(n) {
  if (n <= 10) return(n)

  caminho <- c()

  if (n <= 39) {
    # Para n entre 11 e 39, saltamos de 4 em 4 começando do 10
    caminho <- seq(10, n, by = 4)
  } else {
    # Para páginas muito altas, clicamos no 'Last Page' para vir de trás para frente
    caminho <- c("Last")
    # Após ir para a última, saltamos de 5 em 5 voltando (ou direto se estiver perto)
    if (n < 70) {
      caminho <- c(caminho, seq(70, n, by = -5))
    }
  }

  # Garante que o alvo final esteja na lista e remove duplicatas
  if (tail(caminho, 1) != n) caminho <- c(caminho, n)
  return(unique(caminho))
}
navegar_veloz <- function(b, alvo) {
  passos <- gerar_sequencia_clique(alvo)
  #message(paste("Rota de navegação para página", alvo, ":", paste(passos, collapse = " -> ")))

  for (p in passos) {
    if (p == "Last") {
      # Clica no botão de última página
      selector <- "a.ui-paginator-last"
    } else {
      # Clica no número da página específica
      selector <- sprintf("a.ui-paginator-page[aria-label='Page %s']", p)
    }

    # Executa o clique via JS
    cmd <- sprintf("document.querySelector(\"%s\").click();", selector)
    b$Runtime$evaluate(cmd)

    # Espera curta para o AJAX do site processar o salto
    Sys.sleep(2.5)
  }

  # Verificação final de sincronia
  check_final <- sprintf("document.querySelector(\".ui-state-active[aria-label='Page %s']\") !== null", alvo)
  confirmacao <- b$Runtime$evaluate(check_final)$result$value

  if (confirmacao) {
    message(paste("Sincronizado na página", alvo))
    return(TRUE)
  } else {
    #message("Falha na sincronização do salto.")
    return(FALSE)
  }
}


#### Web scrapping ####

# Iniciar nova sessão no chrome
b <- ChromoteSession$new()

# Definir url
url_base <- "https://www.webambiente.cnptia.embrapa.br/publico/especies.xhtml;jsessionid=I2N0sjaBm4L-6770Qc7ZG5TTPZ3e8bnWFFk4q55P.virt0041"

# Abri site
b$view()
b$go_to(url_base, wait_ = FALSE)
b$Page$navigate(url_base)


# Criar lista para salvar resultados
resultados_lista <- list()

# Configurações do looping
pagina_atual <- 16  # Alterar manualmente se quiser começar de outra página
i_atual <- 1       # Alterar manualmente se quiser começar de outra espécie
max_paginas <- 79

while (pagina_atual <= max_paginas) {

  if (pagina_atual > 1) {
    sucesso_nav <- navegar_veloz(b, pagina_atual)
    if (!sucesso_nav) {
      message("Não foi possível encontrar a página na paginação.")
      next
    }
  }


  # Quantas espécies existem na página? (geralmente 10)
  res_count <- b$Runtime$evaluate("document.querySelectorAll('tbody[id*=\"selectEspecieFormTabela\"] tr').length;")
  total_na_pagina <- res_count$result$value

  for (i in i_atual:total_na_pagina) {

    resultado_extracao <- tryCatch({
      message(paste("Página", pagina_atual, "| Espécie", i, "/", total_na_pagina))

      # Clica na espécie i (índice do JS começa em 0, por isso i-1)
      script_clique <- sprintf("
    (function() {
      var row = document.querySelectorAll('tr.ui-widget-content')[%d];
      if (row) {
        var cell = row.querySelector('td'); // Tenta a primeira célula
        if (cell) {
          cell.click();
          return 'Clique enviado à célula';
        }
        row.click(); // Fallback: tenta a linha se a célula falhar
        return 'Clique enviado à linha';
      }
      return 'Linha não encontrada';
    })()
  ", i - 1)

      # Aguardar tabela carregar
      tentativa_tabela <- 0
      tabela_carregada <- FALSE

      while(tentativa_tabela < 15 && !tabela_carregada) {
        # Seletor via classe CSS
        check_script <- "document.querySelectorAll('.ui-datatable-data tr').length;"
        check_rows <- b$Runtime$evaluate(check_script)$result$value

        if (!is.null(check_rows) && check_rows > 0) {
          tabela_carregada <- TRUE
        } else {
          Sys.sleep(1)
          tentativa_tabela <- tentativa_tabela + 1
        }
      }

      if (!tabela_carregada) {
        # Se falhar, vamos tentar dar um refresh na consulta antes de desistir
        b$Runtime$evaluate("document.querySelector(\"input[value='Consultar']\").click();")
        Sys.sleep(5)
        stop("A tabela não carregou. O robô tentou re-clicar em Consultar.")
      }

      # Script de clique
      script_clique <- sprintf("
        (function() {
          // Seleciona as linhas pela classe padrão do PrimeFaces
          var rows = document.querySelectorAll('.ui-datatable-data tr');
          var row = rows[%d];
          if (row) {
            // Clica na primeira célula da linha i
            var cell = row.querySelector('td');
            if (cell) {
              cell.click();
              return 'Clique enviado à célula';
            }
          }
          return 'Linha não encontrada (Total: ' + rows.length + ')';
        })()
      ", i - 1)

      status_clique <- b$Runtime$evaluate(expression = script_clique)$result$value
      message(paste("Status do navegador:", status_clique))
      Sys.sleep(4)

      # EXTRAÇÃO
      # Pega o HTML da página de detalhes
      html_source <- b$Runtime$evaluate("document.documentElement.outerHTML;")$result$value
      html_interno <- read_html(html_source)

      # Extração
      dados_sp <- extrair_especie_com_na(html_interno)

      # Salvar dados da espécie
      spp_name <- florabr::get_binomial(dados_sp$Espécie,
                                        include_variety = FALSE,
                                        include_subspecies = FALSE)
      file_name <- paste0("Pag_", pagina_atual, "_Especie_", i,
                          "_", spp_name, ".gz")
      data.table::fwrite(dados_sp,
                         file.path("Especies/", file_name))

      # Voltar para página inicial
      if (pagina_atual > 1) {
        b$Runtime$evaluate("document.querySelector('button[id*=\"j_idt78\"]').click();")
        # b$Page$navigate(url_base)
        sucesso_nav <- navegar_veloz(b, pagina_atual)
        if (!sucesso_nav) {
          # Try again
          sucesso_nav <- navegar_veloz(b, pagina_atual)
          if (!sucesso_nav) {
            message("Não foi possível encontrar a página na paginação.")
          }
        }
      } else {
        b$Runtime$evaluate("document.querySelector('button[id*=\"j_idt78\"]').click();")
      }

      TRUE # Sucesso

    }, error = function(e) {
      message(paste("!!! Falha na espécie", i, ":", e$message))
      FALSE # Falha
    })

  }

  # Se for a última espécie da página, reseta i_atual e avança a página
  if (i == total_na_pagina) {
    pagina_atual <- pagina_atual + 1
    i_atual <- 1
    # Voltar para página inicial
    if (pagina_atual > 1) {
      b$Runtime$evaluate("document.querySelector('button[id*=\"j_idt78\"]').click();")
      sucesso_nav <- navegar_veloz(b, pagina_atual)
      if (!sucesso_nav) {
        # Try again
        sucesso_nav <- navegar_veloz(b, pagina_atual)
        if (!sucesso_nav) {
          message("Não foi possível encontrar a página na paginação.")
        }
      }
    }
  }
}

#### Import data ####
library(pbapply)
library(data.table)
library(stringr)
library(dplyr)

# Importar dados
lf <- list.files("Especies/", full.names = TRUE)
d <- pblapply(lf, fread) %>% rbindlist()

# Criar coluna com nome binomial
d2 <- d %>%
  mutate(binomial = str_extract(Espécie, "^[A-Z][a-z]+ [a-z]+"), .before = 1) %>%
  mutate(especie = str_replace_all(Espécie, pattern = "([a-z])(?=[A-Z\\(])",
                                   replacement = "\\1 "), .before = 1) %>%
  select(-Espécie)


# Salvar
write.csv(d2, "WebAmbienteEmbrapa.csv", row.names = FALSE)

# Importar
d2 <- read.csv("WebAmbienteEmbrapa.csv")

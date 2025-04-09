# Função para renderizar tabelas
"%>%" <- magrittr::"%>%"

knit_kable <- function(df, caption = "", font_size = 12, position = "center"){
  knitr::kable(df, format = "latex", align = "c",
               escape = FALSE, caption = caption,
               booktabs = TRUE) %>% 
    kableExtra::kable_styling(latex_options = c("striped", "HOLD_position"),
                              font_size = font_size,
                              position = position)
}


grafbarcomb<- function(d,legx,legy,legl=""){
  # d = banco de dados utilizado
  # v1 = variavel categorica 1
  # v2 = variavel categórica 2
  # legx = legenda do eixo x
  # legy = legenda do eixo y
  data.frame(d) |> 
    ggplot() +
    aes(x = Var1, fill = Var2, group = Var2, weight = Freq) +
    geom_bar(position = "dodge",colour="black") +
    scale_fill_brewer(palette = "Set1", direction = 1) +
    labs(
      x = legx,
      y = legy,
      fill = legl
    )+
    theme_minimal()+
    # geom_label(aes(x = Var1, y = Freq, label = Freq))+
    theme(legend.position = "bottom")+
    coord_flip()
    }


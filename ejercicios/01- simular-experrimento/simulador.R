library(ggplot2)
suppressPackageStartupMessages(library(tidyverse))

experimento <- function (n, m_real, sd_real, h1, p)
{

  datos <- data.frame(y = rnorm(n, m_real, sd_real))
 
  es_estimado <-  sd(datos$y) / n
  m_estimada <- mean(datos$y)
  m_h0 = 0
  q_h0_up = qnorm(p = p, mean = 0, sd = es_estimado,  lower.tail = F)
  beta_p = 1 - pnorm(q_h0_up, mean = h1, sd = es_estimado)
  
  x <- seq(from = -3 * es_estimado, 
           to = max(h1 + 3 * es_estimado, m_estimada), 
           length = 1500 )
  
  dist_hs <- data.frame(x = x, 
                        y_h0 = dnorm(x, 0,  sd = es_estimado), 
                        y_h1 = dnorm(x, h1, sd = es_estimado))
  
  etiquetas <- data.frame(
                  x = c(0, h1),
                  y = c(0.5, 0.5),
                  texto = c("H0", "H1"))
  
  grafica <- dist_hs |> 
    ggplot(aes(x = x)) + 
    geom_line(aes(y = y_h0), color = "blue") +
    geom_ribbon(data = dist_hs[x >= q_h0_up,], xmin = q_h0_up, 
                aes(xmax = Inf, ymin = 0, ymax = y_h0), fill = "blue", alpha = 0.3) +
    geom_line(aes(y = y_h1), color = "red") +
    geom_ribbon(data = dist_hs[x <= q_h0_up,], xmin = -Inf, 
                aes(xmax = q_h0_up, ymin = 0, ymax = y_h1), fill = "red", alpha = 0.3) +
    geom_ribbon(data = dist_hs[x >= q_h0_up,], xmin = q_h0_up, 
                aes(xmax = q_h0_up, ymin = 0, ymax = y_h1), fill = "yellow", alpha = 0.3) +
    geom_vline(xintercept = m_estimada, 
               color = "darkgreen",
               linetype = "dotted") +
    geom_label(data = etiquetas, aes(x = x, y = y, label = texto),                 , 
               color="orange", 
               size=7 , angle=45, fontface="bold" ) +
    labs(title = "Experimento simulado", subtitle = "analiza H0 vs H1") +
    ylab(label = "densidad de probabilidad") +
    xlab(label = "variable de respuesta")
  
  resultados <- list(grafica = grafica,
                     resultados = data.frame(h1 = h1,
                           n = n,
                           m_obs = m_estimada,
                           error_es = es_estimado,
                           p_v_obs = 1 - pnorm(q = m_estimada, mean = 0, sd = es_estimado),
                           p_H0 = p,
                           beta_p = beta_p,
                           potencia = 1 - beta_p))
  
  
  return(resultados)
}

experimento(n = 20, m_real = 1.2, sd_real = 6, h1 = 1, p = 0.05)

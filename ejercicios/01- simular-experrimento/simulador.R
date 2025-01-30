library(ggplot2)
suppressPackageStartupMessages(library(tidyverse))


# Función que contiene los términos de la simulación del experimento
experimento <- function (n, m_real, sd_real, h1, p)
{
  # datos simulados
  datos <- data.frame(y = rnorm(n, m_real, sd_real))
 
  # estimadores estadísticos a partir de los datos simulados
  es_estimado <-  sd(datos$y) / sqrt(n)
  m_estimada <- mean(datos$y)
  m_h0 = 0
  q_h0_up = qnorm(p = p, mean = 0, sd = es_estimado,  lower.tail = F)
  beta_p = 1 - pnorm(q_h0_up, mean = h1, sd = es_estimado)
  
  # Valores teóricos de las distribuciones suponiendo H0 y H1
  #intervalo de valores de respuesta explorados
  x <- seq(from = -3 * es_estimado, 
           to = max(h1 + 3 * es_estimado, m_estimada), 
           length = 1500 )
  # valores de densidad de probabilidad asociado a cada valor x
  dist_hs <- data.frame(x = x, 
                        y_h0 = dnorm(x, 0,  sd = es_estimado), 
                        y_h1 = dnorm(x, h1, sd = es_estimado))
  
  # Graficación ilustrativa del experimento
  etiquetas <- data.frame(
                  x = c(0, h1),
                  y = c(0.8, 0.8),
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
               color="gray40", 
               size=6 , angle=45, fontface="bold" ) +
    labs(title = "Experimento simulado", subtitle = "analiza H0 vs H1") +
    ylab(label = "densidad de probabilidad") +
    xlab(label = "variable de respuesta")
  
  # Organización y entrega de resultados
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


# Realiza el experimento
experimento(n = 20, m_real = 1, sd_real = 1, h1 = 0.5, p = 0.05)


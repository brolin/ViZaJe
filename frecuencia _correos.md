# frecuencia_correos
Frecuencia de correos unloquer durante el período 2009-2019

Autora: Eliette Restrepo, 2019.03.07, 11:27 (CET)

Esta gráfica se creó con el archivo de correos de Unloquer durante el período 2009-2019

- Inputs: 
1. Archivo "Correos con contenido", ubicado en: https://raw.githubusercontent.com/brolin/ViZaJe/b02a6267a632d2cc15318a0b1c86a4310a112b7d/Data/2019/CorreosConContenido.csv
- Outputs: 
1. Frecuencia de correos por año (Figura 1, panel A) y 
2. Frecuencia de asuntos en 2014 (Figura 1, panel B)

El código para crear la gráfica se encuentra en https://github.com/elietterestrepo/unloquer_er. Específicamente se usaron dos scripts:
1. "mails_time.R" es el script principal. Contiene cálculos y llama a la función "fun-graph_combo" para visualizar los resultados
2. "fun-graph_combo.R" es una función en R para visualizar varias gráficas en una sola cuadrícula

Resultados:
- Total de correos: 13 441 (unidades enviadas) - Figura 1, panel A
- Año con mayor número de correos: 2014 (2 355 correos)
- Asuntos con mayor número de correos en 2014: "Mañana sábado", "Propuesta 2014", "biohackerspace" - Figure 1, panel B

Análisis: 
- La actividad del grupo incrementó rápidamente entre el 2009 y el 2014. El pico de actividad ocurrió en el año 2014. La actividad del grupo ha disminuido drásticamente desde el 2015. Comparado con el primer año de actividad (2009), en el 2018 la actividad del grupo disminuyó en un 86%.  
- Los asuntos con mayor número de correos en el 2014 fueron "Mañana sábado" y "Propuesta 2014". Estos resultados sugieren que la mayor actividad giró en torno a encuentros para concretizar "Propuesta 2014". 
- Sin embargo, los asuntos con mayor número de correos solo suman aproximadamente 60 de un total de 2 355, lo cual indica que en el 2014 el grupo estuvo activo en una gran variedad de asuntos. 

Trabajo futuro:
- Realizar análisis de texto para entender mejor los temas tratados, personas involucradas, etc. Por ejemplo: realizar análisis de texto de los asuntos, para agruparlos por tema 
- Refinar el análisis de correos enviados. Por ejemplo: fueron correos automáticos, o realmente enviados por personas?
- Otros

Fin :neckbeard:

Nota: este es un trabajo en proceso. ¡Bienvenidas todas las contribuciones y correcciones! 

![Correos por año ](https://github.com/elietterestrepo/unloquer_er/blob/master/e-mail_summary2.png)

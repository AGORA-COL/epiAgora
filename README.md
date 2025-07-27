# Rutina de análisis de datos RIPS para vigilancia epidemiológica en Colombia
Este repositorio contiene una rutina de análisis en software libre, desarrollada en el marco del proyecto ÁGORA, orientada al mejoramiento de la calidad y análisis de los datos del Registro Individual de Prestación de Servicios de Salud (RIPS). Esta herramienta permite agrupar eventos de salud y evaluar tendencias en la atención médica entre 2018 y 2022, con especial énfasis en los cambios observados durante la pandemia por COVID-19.

## Objetivo
Generar rutinas reproducibles para agrupar y analizar eventos de salud registrados en RIPS, facilitando el uso de datos administrativos para estudios de vigilancia en salud pública. El enfoque es adaptable al contexto colombiano y contempla el desarrollo futuro de una librería en R.

## Contenido del repositorio
### data/
Contiene la base de datos suministrada por el Ministerio de Salud y Protección Social. Incluye diagnósticos médicos (CIE-10), procedimientos (CUPS) y características del paciente como edad, sexo y tipo de afiliación.

### scripts/
Incluye análisis preliminares, pruebas de verificación de códigos CIE-10 y CUPS, y exploraciones de métodos para agrupar eventos de salud. También se incluye la estimación de prevalencia aproximada usando la metodología de Charlson.

### functions/
Contiene la rutina principal de análisis:

Limpieza y verificación de datos.

Agrupación de eventos de salud.

Análisis estadístico del impacto de la pandemia sobre estos eventos.
También se encuentran aquí los archivos ui.R y server.R para ejecutar un dashboard interactivo que permite visualizar características clave de las consultas médicas.

output/
Incluye los productos generados por la rutina: agrupaciones, gráficos, tablas y otros resultados.

## Sistemas de agrupación incluidos
### 1. Sistema de Agrupación ÁGORA
Este esquema fue diseñado en el marco del proyecto ÁGORA para abordar necesidades específicas de análisis en Colombia, como el estudio de transiciones epidemiológicas y demográficas.
Agrupa los 12.654 códigos CIE-10 en 15 categorías clínicas, considerando su relevancia en salud pública nacional y permitiendo desagregaciones regionales y temporales.

Categorías principales:

Alteraciones visuales o auditivas (335)

Lesiones o agresiones (4.692)

COVID-19 (5)

Enfermedades cardiovasculares y metabólicas (639)

Infecciones (1.095)

Osteomusculares y degenerativas (528)

Trastornos neurológicos o mentales (726)

Tumores y enfermedades inmunológicas (921)

...y otras

Este sistema permite una clasificación integral, compensando el sesgo por eventos muy frecuentes, y facilitando análisis descriptivos y comparativos robustos.

### 2. Sistema de Agrupación tipo Charlson
Basado en el índice de comorbilidad de Charlson, ampliamente validado a nivel internacional, este sistema agrupa únicamente las comorbilidades crónicas relevantes para la mortalidad y carga de enfermedad, usando 880 códigos CIE-10.

Incluye 17 categorías, tales como:

Insuficiencia cardíaca

Diabetes (con y sin complicaciones)

Enfermedad renal

Enfermedad hepática

Tumores malignos y metastásicos

VIH/SIDA

Demencia

Enfermedad pulmonar crónica

Enfermedades cerebrovasculares, entre otras.

Este sistema no considera códigos asociados a eventos de alta frecuencia pero bajo impacto (como salud oral o COVID-19), ya que fue diseñado antes de la pandemia y está centrado en el pronóstico clínico.

Ambos sistemas son complementarios:

ÁGORA ofrece un enfoque contextualizado y completo para salud pública en Colombia.

Charlson aporta robustez metodológica y comparabilidad internacional para el estudio de comorbilidades crónicas.



## License
This repository is published under the [AGORA](https://github.com/AGORA-COL/.github) project license (Alliance for Generating Evidence on COVID-19, its Response, and Lessons Learned for the Post-Pandemic and Future Epidemics).

### References
[Quan, H., Sundararajan, V., Halfon, P., Fong, A., Burnand, B., Luthi, J. C., Saunders, L. D., Beck, C. A., Feasby, T. E., & Ghali, W. A. (2005). Coding algorithms for defining comorbidities in ICD-9-CM and ICD-10 administrative data. Medical Care, 43(11), 1130–1139] (Quan, H., Sundararajan, V., Halfon, P., Fong, A., Burnand, B., Luthi, J. C., Saunders, L. D., Beck, C. A., Feasby, T. E., & Ghali, W. A. ​​(2005). Coding algorithms for defining comorbidities in ICD-9-CM and ICD-10 administrative data. Medical Care, 43(11), 1130–1139. https://doi.org/10.1097/01.MLR.0000182534.19832.83)

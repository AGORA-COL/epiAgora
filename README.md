# Rutina de análisis de datos RIPS para vigilancia epidemiológica en Colombia
Este repositorio contiene una rutina de análisis en software libre, desarrollada en el marco del proyecto ÁGORA, orientada al mejoramiento de la calidad y análisis de los datos del Registro Individual de Prestación de Servicios de Salud (RIPS). Esta herramienta permite agrupar eventos de salud y evaluar tendencias en la atención médica entre 2018 y 2022, con especial énfasis en los cambios observados durante la pandemia por COVID-19.

## Objetivo
Generar rutinas reproducibles para agrupar y analizar eventos de salud registrados en RIPS, facilitando el uso de datos administrativos para estudios de vigilancia en salud pública. El enfoque es adaptable al contexto colombiano y contempla el desarrollo futuro de una librería en R.

## Contenido del repositorio
### data/
Contiene la base de datos suministrada por el Ministerio de Salud y Protección Social. Incluye diagnósticos médicos (CIE-10) y características del paciente como edad, sexo y tipo de afiliación.

### scripts/
Incluye análisis preliminares, pruebas de verificación de códigos CIE-10 y exploraciones de métodos para agrupar eventos de salud con dos sistemas de clasificaciones de códigos: CHARLSON y ÁGORA.

### functions/
Contiene la rutina principal de análisis:

- Limpieza y verificación de datos.

- Agrupación de eventos de salud.

- Visualizaciones básicas del impacto de la pandemia sobre estos eventos.

### output/
Incluye los productos generados por la rutina: agrupaciones, gráficos, tablas y otros resultados.

## Sistemas de agrupación incluidos
### 1. Sistema de Agrupación ÁGORA
Este esquema fue diseñado en el marco del proyecto ÁGORA para abordar necesidades específicas de análisis en Colombia, como el estudio de transiciones epidemiológicas y demográficas.
Agrupa los 12.654 códigos CIE-10 en 15 categorías clínicas, considerando su relevancia en salud pública nacional y permitiendo desagregaciones regionales y temporales y se distribuyen de la siguiente manera:

- Alteraciones visuales o auditivas (335 códigos).
- Condiciones asociadas a lesiones o agresiones (4.692 códigos).
- COVID-19 (5 códigos).
- Enfermedades cardiovasculares y metabólicas (639 códigos).
- Enfermedades de los sistemas digestivo o urinario (727 códigos).
- Enfermedades infecciosas (1.095 códigos).
- Enfermedades osteomusculares y degenerativas (528 códigos).
- Enfermedades respiratorias crónicas o de la piel o estructuras anexas (460 códigos).
- Factores relacionados con el contacto con los servicios de salud (592 códigos).
- Salud oral (105 códigos).
- Signos y síntomas mal definidos (311 códigos).
- Trastornos materno-perinatales, congénitos o nutricionales (1.501 códigos).
- Trastornos neurológicos o mentales (726 códigos).
- Tumores, enfermedades hematopoyéticas y del sistema inmune (921 códigos).
- No válido para análisis (17 códigos).


### 2. Sistema de Agrupación tipo Charlson
Basado en el índice de comorbilidad de Charlson, ampliamente validado a nivel internacional, este sistema agrupa únicamente las comorbilidades crónicas relevantes para la mortalidad y carga de enfermedad, usando 880 códigos CIE-10.

- Las categorías propuestas para este sistema de agrupación de códigos incluyen:
- Cualquier tipo de malignidad, incluyendo linfoma y leucemia, excepto neoplasias malignas de piel (432 códigos)
- Demencia (23 códigos)
- Diabetes con complicaciones crónicas (25 códigos)
- Diabetes sin complicaciones crónicas (25 códigos)
- Enfermedad cerebrovascular (82 códigos)
- Enfermedad pulmonar crónica (52 códigos)
- Enfermedad hepática leve (33 códigos)
- Enfermedad hepática moderada o severa (11 códigos)
- Enfermedad vascular periférica (25 códigos)
- Enfermedad renal (31 códigos)
- Enfermedades reumáticas (26 códigos) 
- Hemiplejía o paraplejía (19 códigos)
- Insuficiencia cardíaca congestiva (18 códigos)
- Infarto de miocardio (11 códigos)
- SIDA/VIH (22 códigos)
- Tumor sólido metastásico (9 códigos)
- Úlcera péptica (36 códigos)

Por su solidez metodológica y amplia validación en diversas poblaciones, esta agrupación de 17 categorías ha facilitado su incorporación en estudios epidemiológicos (9). En conjunto, ambos sistemas de agrupación ofrecen enfoques complementarios para el análisis epidemiológico: mientras el sistema ÁGORA permite una visión detallada de los eventos en salud pública relevantes para el contexto colombiano, abarcando la totalidad de códigos CIE-10 y priorizando eventos relevantes para el sistema de salud local, la agrupación de Charlson aporta una herramienta estandarizada y centrada en comorbilidades relevantes desde su punto de vista clínico y la carga de la enfermedad. 

## Contribuciones
Las contribuciones son bienvenidas via pull requests.

## Autores: 
-Zulma M. Cucunubá, 
-Jennifer Murillo-Alvarado 
-Jenny Marcela Pinilla Espejo


## Financiación:
Esta investigación fue financiada por el Ministerio de Ciencia, Tecnología e Innovación de Colombia, proyecto ÁGORA: “Alianza para la Generación de Evidencia sobre COVID-19, su Respuesta y Lecciones Aprendidas para la Postpandemia y Futuras Epidemias” (Contrato N° 637-2022).

## Cómo citar este recurso 
Si utilizas esta rutina o sus sistemas de agrupación en tus análisis o publicaciones, por favor citar de la siguiente manera:

"Proyecto ÁGORA. (2025). Rutina de análisis para el agrupamiento de eventos de salud en Colombia a partir de datos RIPS (2018–2022). Alianza para la Generación de Evidencia sobre COVID-19, su Respuesta y Lecciones para la Pospandemia y Epidemias Futuras. Disponible en: https://github.com/AGORA-Colombia/rips-analisis".


### Referencias
[Quan, H., Sundararajan, V., Halfon, P., Fong, A., Burnand, B., Luthi, J. C., Saunders, L. D., Beck, C. A., Feasby, T. E., & Ghali, W. A. (2005). Coding algorithms for defining comorbidities in ICD-9-CM and ICD-10 administrative data. Medical Care, 43(11), 1130–1139] (Quan, H., Sundararajan, V., Halfon, P., Fong, A., Burnand, B., Luthi, J. C., Saunders, L. D., Beck, C. A., Feasby, T. E., & Ghali, W. A. ​​(2005). Coding algorithms for defining comorbidities in ICD-9-CM and ICD-10 administrative data. Medical Care, 43(11), 1130–1139. https://doi.org/10.1097/01.MLR.0000182534.19832.83)

Charlson ME, Pompei P, Ales KL, MacKenzie CR. A new method of classifying prognostic comorbidity in longitudinal studies: development and validation. J Chronic Dis. 1987;40(5):373–83. DOI: 10.1016/0021-9681(87)90171-8


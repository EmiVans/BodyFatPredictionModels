# BodyFatPredictionModels
Prediction models for estimating body fat percentage in males using clinical data. Developed in R with a dataset of 248 patients and 14 clinical variables. Target variable: brozek.

## Predicción en el caso continuo

Disponer de información sobre el porcentaje de grasa corporal en pacientes masculinos es importante para determinar su estado de salud. En muchos casos, los estudios necesarios para obtener estos datos pueden ser tardados o costosos. Una alternativa para solucionar estos problemas es hacer uso de modelos de predicción.

Se proporcionó información de 14 variables clínicas de 248 pacientes masculinos para obtener distintos modelos de predicción que ayuden a predecir el porcentaje de grasa corporal de futuros nuevos pacientes. En la base de datos, la variable asociada al porcentaje de grasa corporal es **brozek** mientras que las variables explicativas son:

- **age**: Edad de la persona.
- **weight**: Peso corporal
- **height**: Altura
- **adipos**: Índice de adiposidad
- **neck**: Circunferencia del cuello
- **chest**: Circunferencia del pecho
- **abdom**: Circunferencia del abdomen a la altura del ombligo y al nivel de la cresta ilíaca.
- **hip**: Circunferencia de la cadera
- **thigh**: Circunferencia del muslo
- **knee**: Circunferencia de la rodilla
- **ankle**: Circunferencia del tobillo
- **biceps**: Circunferencia extendida del bíceps
- **forearm**: Circunferencia del antebrazo
- **wrist**: Circunferencia de la muñeca distal a las apófisis estiloides.

Para construir las reglas de predicción, usamos modelos lineales generalizados para datos continuos con distribución Gaussiana y liga identidad. Para el entrenamiento, utilizamos las herramientas GLM de R y para calcular el poder predictivo usamos el método K-CV con K=5 y como métrica de predicción se utilizó el *minimum square error (MSE)*. A continuación, presentamos los distintos modelos de predicción obtenidos. Las herramientas computacionales usadas para este trabajo se encuentran en el archivo *prediccont.R*.

Comenzamos construyendo modelos con efectos principales, efectos principales con interacciones de segundo orden y efectos principales con variables al cuadrado. El *MSE* calculado en la evaluación del poder predictivo fue de 17.69, 43.73 y 19.63 respectivamente. Es importante notar que en estos casos no se realizó ninguna selección de variables. Esto debe tenerse en cuenta para construir otros modelos, con la esperanza de obtener una regla con el *MSE* más bajo.

Realizando selección de variables por método stepwise a efectos principales, efectos principales con interacciones de segundo orden y efectos principales con variables al cuadrado, obtuvimos que la evaluación del *MSE* del poder predictivo de los modelos obtenidos fue de 17.99, 25.68 y 17.96 respectivamente. Realizando lo mismo que lo anterior en el mismo orden pero para método lasso, obtuvimos que el *MSE* fue de 16.64, 17.05 y 16.89 respectivamente. Por último, construimos un modelo lineal generalizado con distribución Gaussiana y liga identidad con selección de variables mediante método stepwise a logaritmo de efectos principales; el *MSE* calculado para esta regla fue de 17.99. A continuación, presentamos una tabla que resume la información de los distintos modelos GLM obtenidos. Para complementar el resumen, agregamos otras métricas como el *minimum absolute error (MAE)* y el coeficiente de correlación al cuadrado.

| Conjunto de variables                                                                 | Proceso selección variables | Variables seleccionadas                                                                                                                       | MSE   | MAE   | R^2   |
|----------------------------------------------------------------------------------------|-----------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------|-------|-------|-------|
| Efectos principales                                                                     | sin proceso                  | sin selección                                                                                                                                      | 17.69 | 3.44  | 0.70  |
| Efectos principales con interacciones segundo grado                                    | sin proceso                  | sin selección                                                                                                                                      | 43.73 | 5.07  | 0.448 |
| Efectos principales con Variables al cuadrado                                          | sin proceso                  | sin selección                                                                                                                                      | 19.63 | 3.57  | 0.67  |
| Efectos principales                                                                     | stepwise                     | age, adipos, chest, abdom, wrist                                                                                                                   | 17.99 | 3.501 | 0.69  |
| Efectos principales con interacciones segundo grado                                    | stepwise                     | age, abdom, height, wrist, age:wrist                                                                                                               | 25.68 | 3.97  | 0.61  |
| Efectos principales con Variables al cuadrado                                          | stepwise                     | age, abdom, biceps, wrist, I(adipos^2), I(chest^2), I(biceps^2)                                                                                  | 17.96 | 3.50  | 0.691 |
| Efectos principales                                                                     | lasso                        | age, height, neck, abdom, wrist                                                                                                                    | 16.64 | 3.375 | 0.718 |
| Efectos principales con interacciones segundo grado                                    | lasso                        | hip:wrist, chest:knee, neck:wrist, neck:knee, height:wrist, height:chest, height:neck, age:ankle, age:biceps, abdom, thigh, biceps, forearm | 17.05 | 3.409 | 0.717 |
| Efectos principales con Variables al cuadrado                                          | lasso                        | age, neck, abdom, wrist, I(height^2), I(neck^2)                                                                                                   | 16.89 | 3.43  | 0.717 |
| Logaritmo efectos principales                                                           | stepwise                     | log(height), log(abdom), log(wrist)                                                                                                                | 17.99 | 3.5   | 0.696 |

De la tabla anterior, aclaramos que las filas con respuesta 'sin selección' indican que se tomaron todas las variables del conjunto con el que se está trabajando para construir el modelo GLM. Por otra parte, podemos observar que, de acuerdo con las métricas *MSE*, *MAE* y *R^2*, el mejor modelo de predicción de todos los construidos es el que hace uso de efectos principales con selección de variables lasso, pues tiene el *MSE* y *MAE* más bajos y, a su vez, el *R^2* más alto. Por ello, será esta regla la que utilizaremos para predecir el nivel de grasa corporal de futuros nuevos pacientes. Sea `y` = porcentaje de grasa corporal, entonces el modelo de predicción es:

```math
y = -1.66 + (0.03) \cdot age + (-0.309) \cdot height + (-0.037) \cdot neck + (0.67) \cdot abdom + (-1.07) \cdot wrist

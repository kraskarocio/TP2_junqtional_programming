## TP2 Junqtional Programming

*Si se cambia algo mencionado en esta documentación, actualizala*

**ACLARACIÓN:** Los archivos json pueden ser un Map o una List, importante tomarlo encuenta al hacer una función.

## Compile & run
*(por ahora esto esta hardcodeado, pero se de debería ejecutar algo así)*
```bash
# por ahora se ejecuta así
# sbt "runMain JunqtionalApp opt path_to_json"
sbt "runMain JunqtionalApp .f[1] resources/EX1.json"
```
## Directorios
```bash
.
 scala

├── JunqtionalApp # el archivo principal, que une los demás
|  # toma los args dados por el usuario
├── <functions>
│      ├── functionHandler # acá se maneja el input del ususario
│      ├── functions # acá es donde tienen que poner sus funciones
│      └── transformers
├── <mapToJson>
│      └── mapToJson
├── <parser>
│      ├── JsonParser
│      ├── jsonTokenProcessor
│      ├── scanner
│      └── Token
└── <paths>
       ├── getPathResult
       ├── PathToken
       └── tokenize

```
## funciones importantes
### jsonParser
``` scala
import parser.JsonParser.jsonParser
```
- Toma un String, este representa el archivo json ya leído.
``` scala
// ACTUALMENTE está representado así en JunqtionalApp
val pathFile = "resources/EX1.json"
val input = Source.fromFile(pathFile).getLines().mkString
```
- Devuelve un **Map** o una **List** de scala que representará el contenido del archivo Json
##### Ejemplo
``` json
[{"chau" : {"c":  true}}, 9, 10, "hola"]
```
``` scala
List(Map(chau -> Map(c -> true)), 9, 10, hola)
```
### mapToJson
- Toma un **Map** o una **List** (que representan el json) y devuelve un String que represente al mismo en Json
``` scala
// INPUT
List(Map(chau -> Map(c -> true)), 9, 10, hola)

// OUTPUT
"[{"chau" : {"c": true}}, 9, 10, "hola"]"
```
### navigateRecursive
- Toma un **Map** o una **List** (que representan el json) y devuelve un **Map** o **List** en scala que represente el resultado del path *(ir a la explicación de Path para entender qué son)*.
``` Scala
List(Map(chau -> Map(c -> true)), 9, 10, hola)
// PATH pasado por el usuario: .a.b
// OUTPUT:
Map(c -> true)
```




## paths
*(Falta implementar `|`, pero no es obligatorio según los criterios)*

| json                     | path           | res |
|--------------------------|----------------|-----|
| `{'a':{'b':{'c':0}}}`    | `.a.b.c`       | 0   |
| `['a':{'b':[1,2,3]}, 1]` | `.[0].a.b.[2]` | 3   |


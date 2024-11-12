## TP2 Junqtional Programming

*Si se cambia algo mencionado en esta documentación, actualizala*

**ACLARACIÓN:** Los archivos json pueden ser un Map o una List, importante tomarlo encuenta al hacer una función.

## Compile & run
*(por ahora esto esta hardcodeado, pero se de debería ejecutar algo así)*
```bash
# IMPORTANTE: CORRER ESTO ANTES DE EJECUTAR (y no borrar la carpeta proyect)
sbt assembly

```

```bash
# cat archivo_json | java -jar target/scala-3.3.4/junqtional-assembly-0.1.0-SNAPSHOT.jar
cat resources/EX1.json | java -jar target/scala-3.3.4/junqtional-assembly-0.1.0-SNAPSHOT.jar
```

```bash
echo '{"d":1, "value": 2}' | java -jar target/scala-3.3.4/junqtional-assembly-0.1.0-SNAPSHOT.jar
```

```bash
java -jar target/scala-3.3.4/junqtional-assembly-0.1.0-SNAPSHOT.jar < resources/EX1.json
```

## Funciones

### Merge

```bash
# ejemplo
java -jar path_to_jar merge '{"a":1}'
```
### add item

```bash
# java -jar path_to_jar add_item path jsonList
java -jar path_to_jar add_item .[1].a '[1,2]'
```
_Sí el path no es válido se devuelve sin cambios_
### add key

```bash
# java -jar path_to_jar add_key path key value
java -jar path_to_jar add_key .[3] "b" 1
```

## Directorios
```bash
.
 scala

├──     JunqtionalApp
├── <functions>
│      ├── functionHandler # acá se maneja el input del ususario
│      ├── functions       # fuciones
│      └── transformers    # func helpers
├── <mapToJson>
│      └── mapToJson       # transforma de un tipo List() o Map() a un Str
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
pathFile = "[{"chau" : {"c":  true}}, 9, 10, "hola"]"
```
``` scala
// Output de la función
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


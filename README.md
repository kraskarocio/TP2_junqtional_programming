## TP2 Junqtional Programming

## Compile & run

Valid commands to run the code.

```bash
# por ahora se ejecuta así
# sbt "runMain JunqtionalApp opt path_to_json"
sbt "runMain JunqtionalApp .f[1] resources/EX1.json"
```
## paths
| json                     | path           | res |
|--------------------------|----------------|-----|
| `{'a':{'b':{'c':0}}}`    | `.a.b.c`       | 0   |
| `['a':{'b':[1,2,3]}, 1]` | `.[0].a.b.[2]` | 3   |


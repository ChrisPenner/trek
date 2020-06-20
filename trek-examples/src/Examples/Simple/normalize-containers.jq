[.items | .[] | .spec.containers | .[]
| { (.name) : $container}
] | add

.items | .[] | .spec.containers | .[] 
| . as $container 
| .name = .image + "-" + .name

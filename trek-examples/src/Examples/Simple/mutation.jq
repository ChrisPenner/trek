.items[]
| .metadata.name as $podName
| .spec.containers[]
   |= (.name = $podName + "-" + .name | .ports[].containerPort *= 10)

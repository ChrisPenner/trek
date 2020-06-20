def container: "		- \(.name) \(.ports[0].containerPort)";
def pod: "	Pod \(.metadata.name) has the following containers
\([.spec.containers[] | container] | add)
";

"Let me tell you about my k8s setup:
I have some pods
\([.items[] | pod] | add)
"

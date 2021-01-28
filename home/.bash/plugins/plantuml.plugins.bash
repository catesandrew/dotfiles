# if cask_contains_element "plantuml" || \

if brew_contains_element "plantuml"; then
  export PLANTUML_JAR="$(brew --cellar plantuml)/$(brew list --versions plantuml | tr ' ' '\n' | tail -1)/libexec/plantuml.jar"
fi

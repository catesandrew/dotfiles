# 'Java and JAR helper functions'

# extracts the specified JAR file's MANIFEST file and prints it to stdout
# param '1: JAR file to extract the MANIFEST from'
# example 'jar_manifest lib/foo.jar'
function jar-manifest {
  unzip -c $1 META-INF/MANIFEST.MF
}

if cask_contains_element "java6"; then
  export JAVA_6_HOME=$(/usr/libexec/java_home -v1.6)
fi

if cask_contains_element "java7"; then
  export JAVA_7_HOME=$(/usr/libexec/java_home -v1.7)
fi

if cask_contains_element "java8"; then
  export JAVA_8_HOME=$(/usr/libexec/java_home -v1.8)
fi

if cask_contains_element "java"; then
  export JAVA_9_HOME=$(/usr/libexec/java_home -v9)
fi

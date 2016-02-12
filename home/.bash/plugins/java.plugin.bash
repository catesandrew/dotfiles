# 'Java and JAR helper functions'

# extracts the specified JAR file's MANIFEST file and prints it to stdout
# param '1: JAR file to extract the MANIFEST from'
# example 'jar_manifest lib/foo.jar'
function jar-manifest {
  unzip -c $1 META-INF/MANIFEST.MF
}

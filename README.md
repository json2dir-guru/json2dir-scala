# json2dir-scala

json2dir in plain Scala 3 (stdlib + JDK only, hand-written JSON parser): builds the directory tree a JSON document describes.

    scalac -d out json2dir.scala && java -cp out:scala-library-3.9.0.jar json2dir < document.json

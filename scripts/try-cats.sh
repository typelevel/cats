#!/bin/sh
COURSIER_URL=https://raw.githubusercontent.com/alexarchambault/coursier/v1.0.0-M12/coursier
test -e ~/.coursier/cr || (mkdir -p ~/.coursier && curl -s --output ~/.coursier/cr $COURSIER_URL && chmod +x ~/.coursier/cr)
CLASSPATH="$(~/.coursier/cr fetch -q -p \
  \
  org.typelevel:cats_2.11:0.6.0 \
  com.lihaoyi:ammonite-repl_2.11.8:0.5.8 \
  \
)" java ammonite.repl.Main --predef 'import cats._, cats.data._, cats.implicits._'

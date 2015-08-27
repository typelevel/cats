#!/bin/bash

# Example setting to use at command line for testing:
# export TRAVIS_SCALA_VERSION=2.10.5;export TRAVIS_PULL_REQUEST="false";export TRAVIS_BRANCH="master"

export publish_cmd="publishLocal"

if [[ $TRAVIS_PULL_REQUEST == "false" && $TRAVIS_BRANCH == "master" && $(cat version.sbt) =~ "-SNAPSHOT" ]]; then
  export publish_cmd="publish gitSnapshots publish"
  # temporarily disable to stabilize travis
  #if [[ $TRAVIS_SCALA_VERSION = "2.11.7" ]]; then
  #  export publish_cmd="$publish_cmd ghpagesPushSite"
  #fi
fi

sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"

coverage="$sbt_cmd coverage validateJVM coverageReport && bash <(curl -s https://codecov.io/bash)"
scala_js="$sbt_cmd macrosJS/compile coreJS/compile lawsJS/compile && $sbt_cmd testsJS/test && $sbt_cmd freeJS/test && $sbt_cmd stateJS/test"
scala_jvm="$sbt_cmd validateJVM"

run_cmd="$coverage && $scala_js && $scala_jvm $publish_cmd"
eval $run_cmd

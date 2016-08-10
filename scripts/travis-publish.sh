#!/bin/bash

# Build Overview:
# The overall build is split into a number of parts
# 1. The build for coverage is performed. This:
#   a. First enables the coverage processing, and then
#   b. Builds and tests for the JVM using the validateJVM target, and then
#   c. Produces the coverage report, and then
#   d. Clean is run (as part of coverageReport), to clear down the built artifacts
# 2. The scala js build is executed, compiling the application and testing it for scala js.
# 3. The validateJVM target is executed again, due to the fact that producing coverage with the
#    code coverage tool causes the byte code to be instrumented/modified to record the coverage
#    metrics when the tests are executing. This causes the full JVM build to be run a second time.

# Example setting to use at command line for testing:
# export TRAVIS_SCALA_VERSION=2.10.5;export TRAVIS_PULL_REQUEST="false";export TRAVIS_BRANCH="master"

export publish_cmd="publishLocal"

if [[ $TRAVIS_PULL_REQUEST == "false" && $TRAVIS_BRANCH == "master" && $(cat version.sbt) =~ "-SNAPSHOT" ]]; then
  export publish_cmd="publish gitSnapshots publish"
  # temporarily disable to stabilize travis
  #if [[ $TRAVIS_SCALA_VERSION = "2.11.8" ]]; then
  #  export publish_cmd="$publish_cmd ghpagesPushSite"
  #fi
fi

sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"

coverage="$sbt_cmd coverage validateJVM coverageReport && codecov"


scala_js="$sbt_cmd macrosJS/compile coreJS/compile lawsJS/compile && $sbt_cmd kernelLawsJS/test && $sbt_cmd testsJS/test && $sbt_cmd js/test"		
scala_jvm="$sbt_cmd validateJVM"		
  		  
run_cmd="$coverage && $scala_js && $scala_jvm $publish_cmd"

eval $run_cmd
